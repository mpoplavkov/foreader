package ru.poplavkov.foreader.text.impl

import ru.poplavkov.foreader.Globals.WordStr
import ru.poplavkov.foreader.text.impl.ContextExtractorImpl._
import ru.poplavkov.foreader.text.{ContextExtractor, TextContext, Token}

import scala.annotation.tailrec

/**
  * @author mpoplavkov
  */
class ContextExtractorImpl(contextLen: Int) extends ContextExtractor {

  override def extractContext(tokens: Seq[Token], startIndex: Int, endIndex: Int): TextContext = {
    val left = takeWords(tokens, startIndex, contextLen, _ - 1)
    val right = takeWords(tokens, endIndex, contextLen, _ + 1)
    TextContext.SurroundingWords(left, right)
  }

}

object ContextExtractorImpl {

  @tailrec
  private def takeWords(tokens: Seq[Token],
                        index: Int,
                        count: Int,
                        withIndex: Int => Int,
                        alreadyTaken: Seq[WordStr] = Seq.empty): Seq[WordStr] = {
    if (index < 0 || index >= tokens.size || count == 0) {
      alreadyTaken
    } else {
      tokens(index) match {
        case Token.Word(_, _, lemma, _) =>
          takeWords(tokens, withIndex(index), count - 1, withIndex, alreadyTaken :+ lemma)
        case Token.Punctuation(_, mark) if mark.isEndOfSentence =>
          alreadyTaken
        case _ =>
          takeWords(tokens, withIndex(index), count, withIndex, alreadyTaken)
      }
    }
  }

}
