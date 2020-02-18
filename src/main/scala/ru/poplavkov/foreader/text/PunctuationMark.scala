package ru.poplavkov.foreader.text

import enumeratum._

import scala.collection.immutable

/**
  * @author mpoplavkov
  */
sealed trait PunctuationMark extends EnumEntry

object PunctuationMark extends Enum[PunctuationMark] {

  override def values: immutable.IndexedSeq[PunctuationMark] = findValues

  case object Dot extends PunctuationMark

  case object Hyphen extends PunctuationMark

  case object Comma extends PunctuationMark

  case object Semicolon extends PunctuationMark

}
