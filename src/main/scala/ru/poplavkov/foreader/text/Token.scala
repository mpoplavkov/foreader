package ru.poplavkov.foreader.text

/**
  * @author mpoplavkov
  */
sealed trait Token {

  def position: Int

}

object Token {

  case class Word(position: Int, original: String, lemma: String, partOfSpeech: PartOfSpeech) extends Token

  case class Punctuation(position: Int, mark: PunctuationMark) extends Token

}
