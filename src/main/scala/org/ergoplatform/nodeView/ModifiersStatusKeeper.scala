package org.ergoplatform.nodeView

import org.ergoplatform.nodeView.ModifiersStatusKeeper._
import org.ergoplatform.nodeView.history.ErgoHistoryReader
import scorex.core.ModifierId

import scala.collection.concurrent.TrieMap
import scala.collection.mutable

/**
  * Class that keeps modifiers statuses when modifier is known, but is not applied yet.
  */
class ModifiersStatusKeeper() {

  type K = mutable.WrappedArray[Byte]

  // TODO some LRU cache might be useful here to limit this size and remove outdated statuses
  private val statuses: TrieMap[K, ModifiersStatus] = TrieMap[K, ModifiersStatus]()

  def status(id: ModifierId)(history: ErgoHistoryReader): ModifiersStatus = {
    val mKey = key(id)
    statuses.getOrElse(mKey,
      if (history.contains(id)) {
        Applied
      } else {
        Unknown
      }
    )
  }

  def set(id: ModifierId, status: ModifiersStatus): Option[ModifiersStatus] = statuses.put(key(id), status)

  protected def key(id: ModifierId) = new mutable.WrappedArray.ofByte(id)
}


object ModifiersStatusKeeper {

  sealed trait ModifiersStatus

  /**
    * This modifier was never known to our peer
    */
  case object Unknown extends ModifiersStatus

  /**
    * Our node have requested this modifier from other peers
    */
  case object Requested extends ModifiersStatus

  /**
    * Our node have received this modifier from other peers.
    * It might not be in cache yet due to modifier processing workflow.
    */
  case object Received extends ModifiersStatus

  /**
    * This modifier was already applied to history
    */
  case object Applied extends ModifiersStatus

}
