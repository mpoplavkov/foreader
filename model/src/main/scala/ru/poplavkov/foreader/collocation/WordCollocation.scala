package ru.poplavkov.foreader.collocation

import ru.poplavkov.foreader.Globals.WordStr
import ru.poplavkov.foreader.text.TextContext

/**
  * Describes some word's collocations, extracted from the context
  *
  * @author mpoplavkov
  */
sealed trait WordCollocation

object WordCollocation {

  def fromContext(context: TextContext, k: Int): Set[WordCollocation] = context match {
    case TextContext.SurroundingWords(before, after) =>
      val next = after.headOption
      val secondNext = after.drop(1).headOption
      val prev = before.lastOption
      val secondPrev = before.dropRight(1).lastOption

      val set: Set[Traversable[WordCollocation]] = Set(
        next.map(NextWord),
        prev.map(PrevWord),
        (next, secondNext).pairMap(NextTwoWords),
        (prev, secondPrev).pairMap(PrevTwoWords),
        (prev, next).pairMap(SurroundingWords),
        (before.takeRight(k) ++ after.take(k)).map(KWindowWord)
      )
      set.flatten
  }

  case class NextWord(word: WordStr) extends WordCollocation

  case class PrevWord(word: WordStr) extends WordCollocation

  case class NextTwoWords(first: WordStr, second: WordStr) extends WordCollocation

  case class PrevTwoWords(first: WordStr, second: WordStr) extends WordCollocation

  case class SurroundingWords(prev: WordStr, next: WordStr) extends WordCollocation

  /**
    * This collocations describes that the `word` is in the +-k offset
    * of the target word
    */
  case class KWindowWord(word: WordStr) extends WordCollocation

  implicit class RichPairOption[A, B](val tuple: (Option[A], Option[B])) extends AnyVal {
    def pairMap[C](f: (A, B) => C): Option[C] = {
      val (aOpt, bOpt) = tuple
      for {
        a <- aOpt
        b <- bOpt
      } yield f(a, b)
    }
  }

}
