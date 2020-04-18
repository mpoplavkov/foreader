package ru.poplavkov.foreader.text

import ru.poplavkov.foreader.Globals.WordStr

/**
  * Single word, a part of a word, or a chain of words
  * Lexical items can be generally understood to convey a single meaning
  *
  * @author mpoplavkov
  */
sealed trait LexicalItem {

  def context: Option[TextContext]

  def setContext(context: Option[TextContext]): LexicalItem = this match {
    case single: LexicalItem.SingleWord => single.copy(context = context)
    case mwe: LexicalItem.MultiWordExpression => mwe.copy(context = context)
  }

  final def partsOfSpeech: Seq[PartOfSpeech] = fromWords(_.partOfSpeech)

  final def wordTokens: Seq[Token.Word] = fromWords(identity)

  final def lemmas: Seq[WordStr] = fromWords(_.lemma)

  final def originals: Seq[WordStr] = fromWords(_.original)

  private def fromWords[T](f: Token.Word => T): Seq[T] = this match {
    case LexicalItem.SingleWord(word, _) => Seq(f(word))
    case LexicalItem.MultiWordExpression(words, _) => words.map(f)
  }

}

object LexicalItem {

  case class SingleWord(word: Token.Word,
                        context: Option[TextContext] = None) extends LexicalItem

  case class MultiWordExpression(words: Seq[Token.Word],
                                 context: Option[TextContext] = None) extends LexicalItem

}
