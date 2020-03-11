package ru.poplavkov.foreader

/**
  * @author mpoplavkov
  */
sealed trait PartOfSpeech

object PartOfSpeech {

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
