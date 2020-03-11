package ru.poplavkov.foreader

import ru.poplavkov.foreader.Globals.WordStr

/**
  * Single word, a part of a word, or a chain of words
  * Lexical items can be generally understood to convey a single meaning
  *
  * @author mpoplavkov
  */
sealed trait LexicalItem {

  def partsOfSpeech: Seq[PartOfSpeech] = fromWords(_.partOfSpeech)

  def lemmas: Seq[WordStr] = fromWords(_.lemma)

  private def fromWords[T](f: Token.Word => T): Seq[T] = this match {
    case LexicalItem.SingleWord(word) => Seq(f(word))
    case LexicalItem.MultiWordExpression(words) => words.map(f)
  }

}

object LexicalItem {

  case class SingleWord(word: Token.Word) extends LexicalItem

  case class MultiWordExpression(words: Seq[Token.Word]) extends LexicalItem

}
