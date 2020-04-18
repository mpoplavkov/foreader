package ru.poplavkov.foreader.text

import ru.poplavkov.foreader.Generators._
import ru.poplavkov.foreader.SpecBase
import ru.poplavkov.foreader.SpecBase._

/**
  * @author mpoplavkov
  */
class ContextExtractorSpec extends SpecBase {

  "ContextExtractor" should {

    "extract context for single word" in {
      val before = genWords(1, 10)
      val after = genWords(1, 10)
      val word = generate[Token.Word]
      val sentence = (before :+ word) ++ after
      val expectedContext = TextContext.SurroundingWords(
        before = before.map(TextContext.word2Context),
        after = after.map(TextContext.word2Context)
      )
      ContextExtractor(sentence, word) shouldBe expectedContext
    }

    "extract context for MWE" in {
      val before = genWords(1, 10)
      val after = genWords(1, 10)
      val mwe = genWords(2, 5)
      val sentence = before ++ mwe ++ after
      val expectedContext = TextContext.SurroundingWords(
        before = before.map(TextContext.word2Context),
        after = after.map(TextContext.word2Context)
      )
      ContextExtractor(sentence, mwe) shouldBe expectedContext
    }

    "extract context for MWE with words in between" in {
      val before = genWords(1, 10)
      val after = genWords(1, 10)
      val mweStart = genWords(2, 5)
      val between = genWords(1, 3)
      val mweEnd = genWords(2, 5)
      val mwe = mweStart ++ mweEnd
      val sentence = before ++ mweStart ++ between ++ mweEnd ++ after
      val expectedContext = TextContext.SurroundingWords(
        before = before.map(TextContext.word2Context),
        after = (between ++ after).map(TextContext.word2Context)
      )
      ContextExtractor(sentence, mwe) shouldBe expectedContext
    }

    "extract context ignoring punctuation" in {

      def withPunctuation(tokens: Seq[Token]): Seq[Token] =
        tokens.flatMap(token => Seq(token, generate[Token.Punctuation]))

      val before = genWords(1, 10)
      val after = genWords(1, 10)
      val word = generate[Token.Word]
      val sentence = (withPunctuation(before) :+ word) ++ withPunctuation(after)
      val expectedContext = TextContext.SurroundingWords(
        before = before.map(TextContext.word2Context),
        after = after.map(TextContext.word2Context)
      )
      ContextExtractor(sentence, word) shouldBe expectedContext

    }
  }
}
