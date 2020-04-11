package ru.poplavkov.foreader.text

import io.circe.{Decoder, Encoder}

/**
  * @author mpoplavkov
  */
sealed trait PartOfSpeech

object PartOfSpeech {

  def all: Set[PartOfSpeech] =
    Set(Noun, Pronoun, Verb, Adjective, Adverb, Preposition, Conjunction, Interjection, Numeral, Other(""))

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

  def stringify(pos: PartOfSpeech): String = pos match {
    case Noun => "noun"
    case Pronoun => "pronoun"
    case Verb => "verb"
    case Adjective => "adjective"
    case Adverb => "adverb"
    case Preposition => "preposition"
    case Conjunction => "conjunction"
    case Interjection => "interjection"
    case Numeral => "numeral"
    case Other(tag) => s"other_$tag"
  }

  def fromString(str: String): Option[PartOfSpeech] = {
    val otherRegexp = "other_(.*)".r
    str match {
      case "noun" => Some(Noun)
      case "pronoun" => Some(Pronoun)
      case "verb" => Some(Verb)
      case "adjective" => Some(Adjective)
      case "adverb" => Some(Adverb)
      case "preposition" => Some(Preposition)
      case "conjunction" => Some(Conjunction)
      case "interjection" => Some(Interjection)
      case "numeral" => Some(Numeral)
      case otherRegexp(tag) => Some(Other(tag))
      case _ => None
    }
  }

  implicit val encoder: Encoder[PartOfSpeech] =
    Encoder.encodeString.contramap(stringify)

  implicit val decoder: Decoder[PartOfSpeech] =
    Decoder.decodeString.emap(s => fromString(s).toRight("PartOfSpeech"))
}
