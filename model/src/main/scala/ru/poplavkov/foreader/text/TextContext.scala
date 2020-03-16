package ru.poplavkov.foreader.text

import ru.poplavkov.foreader.Globals.WordStr

/**
  * @author mpoplavkov
  */
sealed trait TextContext

object TextContext {

  case object Empty extends TextContext

  case class SurroundingWords(before: Seq[WordStr], after: Seq[WordStr]) extends TextContext

  object SurroundingWords {

    val Empty: SurroundingWords = SurroundingWords(Seq.empty, Seq.empty)

  }

}
