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

  implicit val encoder: Encoder[PartOfSpeech] = Encoder.encodeString.contramap {
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

  implicit val decoder: Decoder[PartOfSpeech] = {
    val otherRegexp = "other_(.*)".r
    Decoder.decodeString.map {
      case "noun" => Noun
      case "pronoun" => Pronoun
      case "verb" => Verb
      case "adjective" => Adjective
      case "adverb" => Adverb
      case "preposition" => Preposition
      case "conjunction" => Conjunction
      case "interjection" => Interjection
      case "numeral" => Numeral
      case otherRegexp(tag) => Other(tag)
      case _ => ???
    }
  }

}
