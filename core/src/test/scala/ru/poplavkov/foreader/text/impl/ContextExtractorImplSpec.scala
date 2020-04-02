package ru.poplavkov.foreader.text.impl

import org.scalacheck.Gen
import ru.poplavkov.foreader.Generators._
import ru.poplavkov.foreader.SpecBase
import ru.poplavkov.foreader.SpecBase._
import ru.poplavkov.foreader.text.impl.ContextExtractorImplSpec._
import ru.poplavkov.foreader.text.{TextContext, Token}

/**
  * @author mpoplavkov
  */
class ContextExtractorImplSpec extends SpecBase {

  private val contextLen = generate(Gen.chooseNum(2, 5))
  private val contextExtractor = new ContextExtractorImpl(contextLen)

  "ContextExtractorImpl" should {

    "extract context for single word by index" in {
      val before = genWords(1, 10)
      val after = genWords(1, 10)
      val word = generate[Token.Word]
      val all = (before :+ word) ++ after
      val wordInd = before.length
      val expectedContext = TextContext.SurroundingWords(
        before = before.takeRight(contextLen).map(_.lemma),
        after = after.take(contextLen).map(_.lemma)
      )
      contextExtractor.extractContext(all, wordInd) shouldBe expectedContext
    }

    "extract context for MWE by two indices" in {
      val before = genWords(1, 10)
      val after = genWords(1, 10)
      val mwe = genWords(2, 5)
      val all = before ++ mwe ++ after
      val startInd = before.length
      val endInd = startInd + mwe.length - 1
      val expectedContext = TextContext.SurroundingWords(
        before = before.takeRight(contextLen).map(_.lemma),
        after = after.take(contextLen).map(_.lemma)
      )
      contextExtractor.extractContext(all, startInd, endInd) shouldBe expectedContext
    }

    "extract context by two word sequences" in {
      val before = genWords(1, 10)
      val after = genWords(1, 10)
      val expectedContext = TextContext.SurroundingWords(
        before = before.takeRight(contextLen).map(_.lemma),
        after = after.take(contextLen).map(_.lemma)
      )
      contextExtractor.extractContext(before.reverse, after) shouldBe expectedContext
    }

    "extract context while not end of the sentence" in {
      val start = genWords(1, 10)
      val toContextLeft = generate[Token.Word]
      val before = start :+ punctuationToken(endOfSentence = true) :+ toContextLeft
      val end = genWords(1, 10)
      val toContextRight = generate[Token.Word]
      val after = toContextRight +: punctuationToken(endOfSentence = true) +: end

      val expectedContext = TextContext.SurroundingWords(
        before = Seq(toContextLeft.lemma),
        after = Seq(toContextRight.lemma)
      )
      contextExtractor.extractContext(before.reverse, after) shouldBe expectedContext
    }

    "skip not end of sentence punctuation marks while extracting context" in {
      val before = genWords(3, 10)
      val after = genWords(3, 10)
      val beforeWithPunctuation = before.flatMap(w => Seq(w, punctuationToken(endOfSentence = false)))
      val afterWithPunctuation = after.flatMap(w => Seq(w, punctuationToken(endOfSentence = false)))
      val expectedContext = TextContext.SurroundingWords(
        before = before.takeRight(contextLen).map(_.lemma),
        after = after.take(contextLen).map(_.lemma)
      )
      contextExtractor.extractContext(beforeWithPunctuation.reverse, afterWithPunctuation) shouldBe expectedContext
    }
  }
}

object ContextExtractorImplSpec {

  private def punctuationToken(endOfSentence: Boolean): Token =
    generateSuchThat[Token.Punctuation](_.mark.isEndOfSentence == endOfSentence)

}
