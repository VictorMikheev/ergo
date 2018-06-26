package org.ergoplatform.nodeView.history.storage.modifierprocessors.adproofs

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.history.{ADProofs, BlockTransactions, Extension, Header}
import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.nodeView.history.storage.modifierprocessors.FullBlockProcessor
import scorex.core.consensus.History.ProgressInfo

import scala.util.Try

/**
  * ADProof processor for node regime with DigestState
  */
trait FullProofsProcessor extends ADProofsProcessor with FullBlockProcessor {

  protected val adState: Boolean

  override protected def process(m: ADProofs): ProgressInfo[ErgoPersistentModifier] = {
    typedModifierById[Header](m.headerId)  match {
      case Some(h: Header) =>
        (typedModifierById[BlockTransactions](h.transactionsId), typedModifierById[Extension](h.extensionId)) match {
          case (Some(txs), Some(extension)) if adState =>
            processFullBlock(ErgoFullBlock(h, txs, extension, Some(m)), txsAreNew = false)
          case _ =>
            historyStorage.insert(ByteArrayWrapper(m.id), Seq.empty, Seq(m))
            ProgressInfo(None, Seq.empty, Seq.empty, Seq.empty)
        }
      case _ =>
        throw new Error(s"Header for modifier $m is no defined")
    }
  }

  override protected def validate(m: ADProofs): Try[Unit] = modifierValidation(m, typedModifierById[Header](m.headerId))

}
