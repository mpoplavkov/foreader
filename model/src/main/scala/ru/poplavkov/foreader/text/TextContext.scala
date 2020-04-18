package ru.poplavkov.foreader.text

import ru.poplavkov.foreader.Globals.WordStr

/**
  * @author mpoplavkov
  */
sealed trait TextContext

object TextContext {

  case class SurroundingWords(before: Seq[WordStr], after: Seq[WordStr]) extends TextContext {

    def allWords: Seq[WordStr] = before ++ after

  }

  object SurroundingWords {

    def fromTokens(before: Seq[Token.Word], after: Seq[Token.Word]): SurroundingWords =
      new SurroundingWords(before.map(_.lemma), after.map(_.lemma))

    val Empty: SurroundingWords = SurroundingWords(Seq.empty, Seq.empty)

  }

}
