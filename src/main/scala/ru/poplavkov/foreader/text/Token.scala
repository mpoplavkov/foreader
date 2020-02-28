package ru.poplavkov.foreader.text

import ru.poplavkov.foreader.Globals.WordStr

/**
  * @author mpoplavkov
  */
sealed trait Token {

  def position: Int

}

object Token {

  case class Word(position: Int, original: WordStr, lemma: WordStr, partOfSpeech: PartOfSpeech) extends Token

  case class Punctuation(position: Int, mark: PunctuationMark) extends Token

}
