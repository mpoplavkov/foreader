package ru.poplavkov.foreader.text

import io.circe.{Decoder, Encoder}
import ru.poplavkov.foreader.text.PunctuationMark._

/**
  * @author mpoplavkov
  */
sealed trait PunctuationMark {

  final def isEndOfSentence: Boolean = this match {
    case Dot | Exclamation | Question => true
    case _ => false
  }

  def value: String = this match {
    case Dot | Exclamation | Question | Comma | Colon | Semicolon | Dots | Hyphen => stringify(this)
    case Parenthesis(token) => token
    case Quote(token) => token
    case Other(token) => token
  }

}

object PunctuationMark {

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

  def stringify(mark: PunctuationMark): String = mark match {
    case Dot => "."
    case Exclamation => "!"
    case Question => "?"
    case Comma => ","
    case Colon => ":"
    case Semicolon => ";"
    case Dots => "..."
    case Hyphen => "-"
    case Parenthesis(token) => s"parenthesis_$token"
    case Quote(token) => s"quote_$token"
    case Other(token) => s"other_$token"
  }

  def fromString(str: String): Option[PunctuationMark] = {
    val parenthesisRegexp = "parenthesis_(.*)".r
    val quoteRegexp = "quote_(.*)".r
    val otherRegexp = "other_(.*)".r
    str match {
      case "." => Some(Dot)
      case "!" => Some(Exclamation)
      case "?" => Some(Question)
      case "," => Some(Comma)
      case ":" => Some(Colon)
      case ";" => Some(Semicolon)
      case "..." => Some(Dots)
      case "-" => Some(Hyphen)
      case parenthesisRegexp(token) => Some(Parenthesis(token))
      case quoteRegexp(token) => Some(Quote(token))
      case otherRegexp(token) => Some(Other(token))
      case _ => None
    }
  }

  implicit val encoder: Encoder[PunctuationMark] =
    Encoder.encodeString.contramap(stringify)

  implicit val decoder: Decoder[PunctuationMark] =
    Decoder.decodeString.emap(s => fromString(s).toRight("PunctuationMark"))

}
