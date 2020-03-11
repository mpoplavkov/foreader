package ru.poplavkov.foreader

import enumeratum._

import scala.collection.immutable

/**
  * @author mpoplavkov
  */
sealed trait LexicalItemLevel extends EnumEntry

object LexicalItemLevel extends Enum[LexicalItemLevel] {

  override def values: immutable.IndexedSeq[LexicalItemLevel] = findValues

  case object A1 extends LexicalItemLevel

  case object A2 extends LexicalItemLevel

  case object B1 extends LexicalItemLevel

  case object B2 extends LexicalItemLevel

  case object C1 extends LexicalItemLevel

  case object C2 extends LexicalItemLevel

}
