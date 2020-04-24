package ru.poplavkov.foreader.collocation

import org.scalacheck.Gen
import ru.poplavkov.foreader.Generators._
import ru.poplavkov.foreader.Globals.WordStr
import ru.poplavkov.foreader.SpecBase
import ru.poplavkov.foreader.SpecBase._
import ru.poplavkov.foreader.text.TextContext

/**
  * @author mpoplavkov
  */
class WordCollocationSpec extends SpecBase {

  "WordCollocation.fromContext" should {
    "extract all collocations from the full context" in {
      val next = generate[WordStr]
      val secondNext = generate[WordStr]
      val prev = generate[WordStr]
      val secondPrev = generate[WordStr]
      val context = TextContext.SurroundingWords(
        before = Seq(secondPrev, prev),
        after = Seq(next, secondNext)
      )

      val expected = Set(
        WordCollocation.NextWord(next),
        WordCollocation.PrevWord(prev),
        WordCollocation.NextTwoWords(next, secondNext),
        WordCollocation.PrevTwoWords(prev, secondPrev),
        WordCollocation.SurroundingWords(prev, next),
        WordCollocation.KWindowWord(next),
        WordCollocation.KWindowWord(secondNext),
        WordCollocation.KWindowWord(prev),
        WordCollocation.KWindowWord(secondPrev)
      )

      WordCollocation.fromContext(context, 2) shouldBe expected
    }

    "extract nothing from an empty context" in {
      val context = TextContext.SurroundingWords.Empty
      WordCollocation.fromContext(context, 2) shouldBe Set.empty
    }

    "extract not all collocations" in {

      val next = generate[WordStr]
      val secondNext = generate[WordStr]
      val context = TextContext.SurroundingWords(
        before = Seq.empty,
        after = Seq(next, secondNext)
      )

      val expected = Set(
        WordCollocation.NextWord(next),
        WordCollocation.NextTwoWords(next, secondNext),
        WordCollocation.KWindowWord(next),
        WordCollocation.KWindowWord(secondNext)
      )

      WordCollocation.fromContext(context, 2) shouldBe expected
    }

    "extract only K nearest words" in {
      val after = genWords(5, 10).map(_.lemma)
      val before = genWords(5, 10).map(_.lemma)
      val k = generate[Int](Gen.chooseNum(1, 4))
      val context = TextContext.SurroundingWords(before, after)

      val expectedKWindow =
        (after.take(k) ++ before.takeRight(k))
          .map(WordCollocation.KWindowWord)
          .toSet

      val actualKWindow = WordCollocation.fromContext(context, k).collect {
        case k: WordCollocation.KWindowWord => k
      }

      actualKWindow shouldBe expectedKWindow
    }
  }
}
