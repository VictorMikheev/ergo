package org.ergoplatform.nodeView.history.storage.modifierprocessors.blocktransactions

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.history.{ADProofs, BlockTransactions, Extension, Header}
import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.nodeView.history.storage.modifierprocessors.FullBlockProcessor
import scorex.core.consensus.History.ProgressInfo

import scala.util.Try

/**
  * BlockTransactions processor for settings with verifyTransactions=true
  */
trait FullBlockTransactionsProcessor extends BlockTransactionsProcessor with FullBlockProcessor {
  protected val historyStorage: HistoryStorage

  protected val adState: Boolean

  override protected def process(txs: BlockTransactions): ProgressInfo[ErgoPersistentModifier] = {
    typedModifierById[Header](txs.headerId) match {
      case Some(h: Header) =>
        (typedModifierById[ADProofs](h.transactionsId), typedModifierById[Extension](h.extensionId)) match {
          case _ if bestFullBlockIdOpt.isEmpty && !isValidFirstFullBlock(h) =>
            justPutToHistory(txs)
          case (Some(adProof), Some(extension)) =>
            processFullBlock(ErgoFullBlock(h, txs, extension, Some(adProof)), txsAreNew = true)
          case (None, Some(extension)) if !adState =>
            processFullBlock(ErgoFullBlock(h, txs, extension, None), txsAreNew = true)
          case _ =>
            justPutToHistory(txs)
        }
      case _ =>
        throw new Error(s"Header for modifier $txs is no defined")
    }
  }

  private def justPutToHistory(txs: BlockTransactions): ProgressInfo[ErgoPersistentModifier] = {
    historyStorage.insert(ByteArrayWrapper(txs.id), Seq.empty, Seq(txs))
    ProgressInfo(None, Seq.empty, Seq.empty, Seq.empty)
  }

  override protected def validate(m: BlockTransactions): Try[Unit] =
    modifierValidation(m, typedModifierById[Header](m.headerId))

}
