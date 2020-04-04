package ru.poplavkov.foreader.text

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
    case PunctuationMark.Dot => "."
    case PunctuationMark.Exclamation => "!"
    case PunctuationMark.Question => "?"
    case PunctuationMark.Comma => ","
    case PunctuationMark.Colon => ":"
    case PunctuationMark.Semicolon => ";"
    case PunctuationMark.Dots => "..."
    case PunctuationMark.Hyphen => "-"
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

}
