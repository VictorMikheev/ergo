package org.ergoplatform.local

import java.util

import akka.actor.{Actor, ActorRef, ActorRefFactory, PoisonPill, Props}
import io.circe.Encoder
import io.circe.syntax._
import io.iohk.iodb.ByteArrayWrapper
import org.bouncycastle.util.BigIntegers
import org.ergoplatform.ErgoBox.{R4, TokenId}
import org.ergoplatform.mining.CandidateBlock
import org.ergoplatform.mining.difficulty.RequiredDifficulty
import org.ergoplatform.mining.emission.CoinsEmission
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.ErgoReadersHolder.{GetReaders, Readers}
import org.ergoplatform.nodeView.history.ErgoHistoryReader
import org.ergoplatform.nodeView.mempool.ErgoMemPoolReader
import org.ergoplatform.nodeView.state.{ErgoState, UtxoStateReader}
import org.ergoplatform.settings.{Algos, Constants, ErgoSettings}
import org.ergoplatform._
import scapi.sigma.DLogProtocol.DLogProverInput
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.SemanticallySuccessfulModifier
import scorex.core.utils.{NetworkTimeProvider, ScorexLogging}
import scorex.crypto.hash.Digest32
import sigmastate.SBoolean
import sigmastate.Values.{LongConstant, TrueLeaf, Value}
import sigmastate.interpreter.{ContextExtension, ProverResult}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}


class ErgoMiner(ergoSettings: ErgoSettings,
                viewHolderRef: ActorRef,
                readersHolderRef: ActorRef,
                timeProvider: NetworkTimeProvider,
                emission: CoinsEmission) extends Actor with ScorexLogging {

  import ErgoMiner._

  private val startTime = timeProvider.time()

  //shared mutable state
  private var isMining = false
  private var candidateOpt: Option[CandidateBlock] = None
  private val miningThreads: mutable.Buffer[ActorRef] = new ArrayBuffer[ActorRef]()

  private val minerProp: Value[SBoolean.type] = {
    //TODO extract from wallet when it will be implemented
    DLogProverInput(BigIntegers.fromUnsignedByteArray(ergoSettings.scorexSettings.network.nodeName.getBytes())).publicImage
  }

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[SemanticallySuccessfulModifier[_]])
  }

  override def postStop(): Unit = killAllThreads

  private def killAllThreads: Unit = {
    log.warn("Stopping miner's threads.")
    miningThreads.foreach(_ ! PoisonPill)
    miningThreads.clear()
  }

  private def unknownMessage: Receive = {
    case m =>
      log.warn(s"Unexpected message $m")
  }

  private def miningStatus: Receive = {
    case MiningStatusRequest =>
      sender ! MiningStatusResponse(isMining, candidateOpt)
  }

  private def startMining: Receive = {
    case StartMining if candidateOpt.nonEmpty && !isMining && ergoSettings.nodeSettings.mining =>
      log.info("Starting Mining")
      isMining = true
      miningThreads += ErgoMiningThread(ergoSettings, viewHolderRef, candidateOpt.get, timeProvider)(context)
      miningThreads.foreach(_ ! candidateOpt.get)
    case StartMining if candidateOpt.isEmpty =>
      requestCandidate
      context.system.scheduler.scheduleOnce(1.seconds, self, StartMining)(context.system.dispatcher)
  }

  private def needNewCandidate(b: ErgoFullBlock): Boolean = {
    val parentHeaderIdOpt = candidateOpt.flatMap(_.parentOpt).map(_.id)
    !parentHeaderIdOpt.exists(parentHeaderId => util.Arrays.equals(parentHeaderId, b.header.id))
  }

  private def shouldStartMine(b: ErgoFullBlock): Boolean = {
    ergoSettings.nodeSettings.mining && b.header.timestamp >= startTime
  }

  private def receiveSemanticallySuccessfulModifier: Receive = {
    /**
      * Case when we are already mining by the time modifier arrives and
      * get block from node view that has header's id which isn't equals to our candidate's parent id.
      * That means that our candidate is outdated. Should produce new candidate for ourselves.
      * Stop all current threads and re-run them with newly produced candidate.
      */
    case SemanticallySuccessfulModifier(mod: ErgoFullBlock) if isMining && needNewCandidate(mod) => requestCandidate

    /**
      * Non obvious but case when mining is enabled, but miner doesn't started yet. Initialization case.
      * We've received block that been generated by somebody else or genesis while we doesn't start.
      * And this block was generated after our miner had been started. That means that we are ready
      * to start mining.
      * This block could be either genesis or generated by another node.
      */
    case SemanticallySuccessfulModifier(mod: ErgoFullBlock) if shouldStartMine(mod) => self ! StartMining

    /**
      * Just ignore all other modifiers.
      */
    case SemanticallySuccessfulModifier(_) =>
  }

  override def receive: Receive = receiveSemanticallySuccessfulModifier orElse
    miningStatus orElse
    startMining orElse
    onReaders orElse
    unknownMessage

  private def onReaders: Receive = {
    case Readers(h, s, m) if s.isInstanceOf[UtxoStateReader] =>
      createCandidate(h, m, s.asInstanceOf[UtxoStateReader]) match {
        case Success(candidate) => procCandidateBlock(candidate)
        case Failure(e) => log.warn("Failed to produce candidate block.", e)
      }
  }

  private def procCandidateBlock(c: CandidateBlock): Unit = {
    log.debug(s"Got candidate block at height ${c.parentOpt.map(_.height).getOrElse(-1) + 1}" +
      s" with ${c.transactions.size} transactions")
    candidateOpt = Some(c)
    miningThreads.foreach(_ ! c)
  }

  private def createCandidate(history: ErgoHistoryReader,
                              pool: ErgoMemPoolReader,
                              state: UtxoStateReader): Try[CandidateBlock] = Try {
    val bestHeaderOpt: Option[Header] = history.bestFullBlockOpt.map(_.header)

    //only transactions valid from against the current utxo state we take from the mem pool
    // todo: size should be limitedby network, size limit should be chosen by miners votes. fix after voting implementation
    val maxBlockSize = 512 * 1024 // honest miner is generating a block of no more than 512Kb
    var totalSize = 0
    val externalTransactions = state.filterValid(pool.unconfirmed.values.toSeq).takeWhile { tx =>
      totalSize = totalSize + tx.bytes.length
      totalSize <= maxBlockSize
    }

    //we also filter transactions which are trying to spend the same box. Currently, we pick just the first one
    //of conflicting transaction. Another strategy is possible(e.g. transaction with highest fee)
    //todo: move this logic to MemPool.put? Problem we have now is that conflicting transactions are still in
    // the pool
    val txsNoConflict = fixTxsConflicts(externalTransactions)

    // TODO use wallet to extract boxes from transactions in this block miner can spend. Use wallet when create coinbase
    val feeBoxes: Seq[ErgoBox] = ErgoState.boxChanges(txsNoConflict)._2.filter(_.proposition == TrueLeaf)
    val coinbase = ErgoMiner.createCoinbase(state, feeBoxes, minerProp, emission)
    val txs = txsNoConflict :+ coinbase

    val (adProof, adDigest) = state.proofsForTransactions(txs).get

    val timestamp = timeProvider.time()

    val nBits: Long = bestHeaderOpt
      .map(parent => history.requiredDifficultyAfter(parent))
      .map(d => RequiredDifficulty.encodeCompactBits(d))
      .getOrElse(Constants.InitialNBits)

    //TODO real extension should be there. Hash from empty array for now to be able to implement it later without forks
    val extensionHash = Algos.hash(Array[Byte]())

    CandidateBlock(bestHeaderOpt, nBits, adDigest, adProof, txs, timestamp, extensionHash)
  }

  def requestCandidate: Unit = readersHolderRef ! GetReaders

  private def fixTxsConflicts(txs: Seq[ErgoTransaction]): Seq[ErgoTransaction] = txs
    .foldLeft((Seq.empty[ErgoTransaction], Set.empty[ByteArrayWrapper])) { case ((s, keys), tx) =>
      val bxsBaw = tx.inputs.map(_.boxId).map(ByteArrayWrapper.apply)
      if (bxsBaw.forall(k => !keys.contains(k)) && bxsBaw.size == bxsBaw.toSet.size) {
        (s :+ tx) -> (keys ++ bxsBaw)
      } else {
        (s, keys)
      }
    }._1
}


object ErgoMiner extends ScorexLogging {

  def createCoinbase(state: UtxoStateReader,
                     feeBoxes: Seq[ErgoBox],
                     minerProp: Value[SBoolean.type],
                     emission: CoinsEmission): ErgoTransaction = {
    state.emissionBoxOpt match {
      case Some(emissionBox) =>
        assert(state.boxById(emissionBox.id).isDefined, s"Emission box ${Algos.encode(emissionBox.id)} missed")
        ErgoMiner.createCoinbase(emissionBox, state.stateContext.height, feeBoxes, minerProp, emission)
      case None =>
        val inputs = feeBoxes
          .map(b => new Input(b.id, ProverResult(Array.emptyByteArray, ContextExtension.empty)))
        val feeAmount = feeBoxes.map(_.value).sum
        val feeTokens = feeBoxes.flatMap(_.additionalTokens).take(ErgoBox.MaxTokens)
        val rewardBox: ErgoBoxCandidate = new ErgoBoxCandidate(feeAmount, minerProp, feeTokens, Map())
        ErgoTransaction(inputs.toIndexedSeq, IndexedSeq(rewardBox))
    }
  }

  def createCoinbase(emissionBox: ErgoBox,
                     height: Int,
                     feeBoxes: Seq[ErgoBox],
                     minerProp: Value[SBoolean.type],
                     emission: CoinsEmission): ErgoTransaction = {
    feeBoxes.foreach(b => assert(b.proposition == TrueLeaf, s"Trying to create coinbase from protected fee box $b"))
    val prop = emissionBox.proposition
    val emissionAmount = emission.emissionAtHeight(height)
    val newEmissionBox: ErgoBoxCandidate =
      new ErgoBoxCandidate(emissionBox.value - emissionAmount, prop, Seq(), Map(R4 -> LongConstant(height)))
    val inputBoxes = (emissionBox +: feeBoxes).toIndexedSeq
    val inputs = inputBoxes
      .map(b => new Input(b.id, ProverResult(Array.emptyByteArray, ContextExtension.empty)))

    val feeAmount = feeBoxes.map(_.value).sum

    val feeAssets = feeBoxes.flatMap(_.additionalTokens).take(ErgoBox.MaxTokens - 1)
    //todo: a miner is creating a new asset, remove it after playing for a while
    val newAsset: (TokenId, Long) = (Digest32 @@ inputBoxes.head.id) -> 1000
    val minerAssets = feeAssets :+ newAsset

    val minerBox = new ErgoBoxCandidate(emissionAmount + feeAmount, minerProp, minerAssets, Map())

    ErgoTransaction(
      inputs,
      IndexedSeq(newEmissionBox, minerBox)
    )
  }


  case object StartMining

  case object MiningStatusRequest

  case class MiningStatusResponse(isMining: Boolean, candidateBlock: Option[CandidateBlock])

  implicit val jsonEncoder: Encoder[MiningStatusResponse] = (r: MiningStatusResponse) =>
    Map(
      "isMining" -> r.isMining.asJson,
      "candidateBlock" -> r.candidateBlock.asJson
    ).asJson

}

object ErgoMinerRef {

  def props(ergoSettings: ErgoSettings,
            viewHolderRef: ActorRef,
            readersHolderRef: ActorRef,
            timeProvider: NetworkTimeProvider,
            emission: CoinsEmission): Props =
    Props(new ErgoMiner(ergoSettings, viewHolderRef, readersHolderRef, timeProvider, emission))

  def apply(ergoSettings: ErgoSettings,
            viewHolderRef: ActorRef,
            readersHolderRef: ActorRef,
            timeProvider: NetworkTimeProvider,
            emission: CoinsEmission)
           (implicit context: ActorRefFactory): ActorRef =
    context.actorOf(props(ergoSettings, viewHolderRef, readersHolderRef, timeProvider, emission))

  def apply(ergoSettings: ErgoSettings,
            viewHolderRef: ActorRef,
            readersHolderRef: ActorRef,
            timeProvider: NetworkTimeProvider,
            emission: CoinsEmission,
            name: String)
           (implicit context: ActorRefFactory): ActorRef =
    context.actorOf(props(ergoSettings, viewHolderRef, readersHolderRef, timeProvider, emission), name)
}
