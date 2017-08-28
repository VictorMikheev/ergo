package org.ergoplatform.modifiers.mempool

import io.circe.Json
import org.ergoplatform.modifiers.history.{BlockTransactions, Header}
import org.ergoplatform.nodeView.state.ErgoState.Digest
import org.ergoplatform.settings.Algos
import scorex.core.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.serialization.Serializer

case class TransactionIdsForHeader(ids: Seq[ModifierId]) extends MempoolModifier {
  override val modifierTypeId: ModifierTypeId = TransactionIdsForHeader.ModifierTypeId

  override lazy val id: ModifierId = Algos.hash(scorex.core.utils.concatFixLengthBytes(ids))

  override type M = TransactionIdsForHeader

  override lazy val serializer: Serializer[TransactionIdsForHeader] = ???

  override lazy val json: Json = ???

  lazy val rootHash: Digest = BlockTransactions.rootHash(ids)
}

object TransactionIdsForHeader {
  val ModifierTypeId: Byte = 103: Byte

  def validate(txIds: TransactionIdsForHeader, header: Header): Boolean = {
    header.transactionsRoot sameElements txIds.rootHash
  }
}