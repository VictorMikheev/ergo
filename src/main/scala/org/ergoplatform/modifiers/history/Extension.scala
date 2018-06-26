package org.ergoplatform.modifiers.history

import akka.util.ByteString
import com.google.common.primitives.Shorts
import org.ergoplatform.modifiers.ModifierWithDigest
import org.ergoplatform.settings.Algos
import scorex.core.serialization.Serializer
import scorex.core.{ModifierId, ModifierTypeId}
import scorex.crypto.authds.LeafData
import scorex.crypto.hash.Digest32

import scala.annotation.tailrec
import scala.util.Try

/**
  * Extension section of Ergo block
  *
  * @param headerId        - id of corresponding header
  * @param mandatoryFields - mandatory fields. Key size is 4 bytes, value size is known for all peers
  * @param optionalFields  - optional fields. Key size is 32 bytes, value size is at most 1024 bytes
  */
case class Extension(headerId: ModifierId,
                     mandatoryFields: Map[ByteString, ByteString],
                     optionalFields: Map[ByteString, ByteString]) extends ModifierWithDigest {
  override val modifierTypeId: ModifierTypeId = Extension.modifierTypeId

  override def digest: Digest32 = Extension.rootHash(this)

  override type M = Extension

  override def serializer: Serializer[Extension] = ExtensionSerializer

  override def toString: String = {
    s"Extension(${Algos.encode(headerId)}, " +
      s"${mandatoryFields.map(kv => s"${Algos.encode(kv._1)} -> ${Algos.encode(kv._2)}")})" +
      s"${optionalFields.map(kv => s"${Algos.encode(kv._1)} -> ${Algos.encode(kv._2)}")})"
  }

  override def equals(obj: Any): Boolean = obj match {
    case that: Extension =>
      (that.headerId sameElements headerId) &&
        mandatoryFields == that.mandatoryFields &&
        optionalFields == that.optionalFields
    case _ => false
  }
}

object Extension {
  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ (108: Byte)

  def rootHash(e: Extension): Digest32 = {
    val elements: Seq[Array[Byte]] = (e.mandatoryFields ++ e.optionalFields).map { f =>
      val prefix = ByteString(f._1.size.toByte)
      (prefix ++ f._1 ++ f._2).toArray[Byte]
    }.toSeq
    Algos.merkleTreeRoot(LeafData @@ elements)
  }

}

object ExtensionSerializer extends Serializer[Extension] {
  private val Delimeter = ByteString(Array.fill(4)(0: Byte))

  override def toBytes(obj: Extension): Array[Byte] = {
    ByteString(obj.headerId) ++
      obj.mandatoryFields.map(f => f._1 ++ ByteString(Shorts.toByteArray(f._2.size.toShort)) ++ f._2).reduce(_ ++ _) ++
      Delimeter ++
      obj.optionalFields.map(f => f._1 ++ ByteString(Shorts.toByteArray(f._2.size.toShort)) ++ f._2).reduce(_ ++ _)
  }.toArray[Byte]

  override def parseBytes(bytes: Array[Byte]): Try[Extension] = Try {
    val totalLength = bytes.length

    @tailrec
    def parseSection(pos: Int,
                     keySize: Int,
                     acc: Map[ByteString, ByteString]): (Map[ByteString, ByteString], Int) = {
      val key = ByteString(bytes.slice(pos, pos + keySize))
      if (key != Delimeter && pos < totalLength) {
        val length = Shorts.fromByteArray(bytes.slice(pos + keySize, pos + keySize + 2))
        val value = ByteString(bytes.slice(pos + keySize + 2, pos + keySize + 2 + length))
        parseSection(pos + keySize + 2 + length, keySize, acc.updated(key, value))
      } else {
        (acc, pos + keySize)
      }
    }

    val headerId = ModifierId @@ bytes.take(32)
    val (mandatory, newPos) = parseSection(32, 4, Map())
    val (optional, _) = parseSection(newPos, 32, Map())
    Extension(headerId, mandatory, optional)
  }
}
