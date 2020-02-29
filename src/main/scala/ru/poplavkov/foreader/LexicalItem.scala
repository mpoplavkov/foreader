package ru.poplavkov.foreader

import ru.poplavkov.foreader.text.Token

/**
  * Single word, a part of a word, or a chain of words
  * Lexical items can be generally understood to convey a single meaning
  *
  * @author mpoplavkov
  */
sealed trait LexicalItem

object LexicalItem {

  case class SingleWord(word: Token.Word) extends LexicalItem

  case class MultiWordExpression(words: Seq[Token.Word]) extends LexicalItem

}
