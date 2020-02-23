package ru.poplavkov.foreader.text

import enumeratum._

import scala.collection.immutable

/**
  * @author mpoplavkov
  */
sealed trait PartOfSpeech extends EnumEntry

object PartOfSpeech extends Enum[PartOfSpeech] {

  override def values: immutable.IndexedSeq[PartOfSpeech] = findValues

  case object Noun extends PartOfSpeech

  case object Pronoun extends PartOfSpeech

  case object Verb extends PartOfSpeech

  case object Adjective extends PartOfSpeech

  case object Adverb extends PartOfSpeech

  case object Preposition extends PartOfSpeech

  case object Conjunction extends PartOfSpeech

  case object Interjection extends PartOfSpeech

  case object Numeral extends PartOfSpeech

  case class Other(tag: String) extends PartOfSpeech

}
