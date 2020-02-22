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

  case object Exclamation extends PunctuationMark

  case object Question extends PunctuationMark

  case object Comma extends PunctuationMark

  case object Colon extends PunctuationMark

  case object Semicolon extends PunctuationMark

  case object Dots extends PunctuationMark

  case object Hyphen extends PunctuationMark

  case class Parenthesis(token: String) extends PunctuationMark

  case class Quote(token: String) extends PunctuationMark

  case class Other(token: String) extends PunctuationMark

}
