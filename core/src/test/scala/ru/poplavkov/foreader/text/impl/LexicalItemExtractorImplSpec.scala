package ru.poplavkov.foreader.text.impl

import org.scalacheck.Gen
import ru.poplavkov.foreader.Generators._
import ru.poplavkov.foreader.SpecBase
import ru.poplavkov.foreader.SpecBase._
import ru.poplavkov.foreader.dictionary.MweSet
import ru.poplavkov.foreader.text._
import ru.poplavkov.foreader.text.impl.LexicalItemExtractorImplSpec._

/**
  * @author mpoplavkov
  */
class LexicalItemExtractorImplSpec extends SpecBase {

  private val mweSet: MweSet[CovariantId] = mock[MweSet[CovariantId]]

  def lexicalItemExtractor: LexicalItemExtractor[CovariantId] = new LexicalItemExtractorImpl(mweSet)

  "LexicalItemExtractorImpl" should {

    "extract lexical item from one word" in {
      val word = generate[Token.Word]

      doReturn(Set.empty)
        .when(mweSet)
        .getMwesStartingWith(anyObject)

      val expected = Seq(LexicalItem.SingleWord(word, Some(TextContext.SurroundingWords.Empty)))
      lexicalItemExtractor.lexicalItemsFromSentence(Seq(word)) shouldBe expected
    }

    "extract sequence of single words" in {
      val words = genWords(2, 10)

      doReturn(Set.empty)
        .when(mweSet)
        .getMwesStartingWith(anyObject)

      val expected = words.map(LexicalItem.SingleWord(_))
      lexicalItemExtractor.lexicalItemsFromSentence(words).withEmptyContext shouldBe expected
    }

    "extract sequence of single words despite on punctuation" in {
      val punctuation = generate[Token.Punctuation]
      val words1 = genWords(1, 5)
      val words2 = genWords(1, 5)
      val words = words1 ++ words2
      val sentence = (words1 :+ punctuation) ++ words2

      doReturn(Set.empty)
        .when(mweSet)
        .getMwesStartingWith(anyObject)

      val expected = words.map(LexicalItem.SingleWord(_))
      lexicalItemExtractor.lexicalItemsFromSentence(sentence).withEmptyContext shouldBe expected
    }

    "extract small multi word expression" in {
      val startingWord = generate[Token.Word]
      val secondWord = generate[Token.Word]
      val sentence = Seq(startingWord, secondWord)

      doReturn(Set(Seq(secondWord.lemma)))
        .when(mweSet)
        .getMwesStartingWith(startingWord.lemma)

      val expected =
        Seq(LexicalItem.MultiWordExpression(Seq(startingWord, secondWord), Some(TextContext.SurroundingWords.Empty)))
      lexicalItemExtractor.lexicalItemsFromSentence(sentence) shouldBe expected

    }

    "extract long multi word expression" in {
      val startingWord = generate[Token.Word]
      val otherWords = genWords(2, 10)
      val sentence = startingWord +: otherWords

      doReturn(Set(otherWords.map(_.lemma)))
        .when(mweSet)
        .getMwesStartingWith(startingWord.lemma)

      val expected = Seq(LexicalItem.MultiWordExpression(sentence, Some(TextContext.SurroundingWords.Empty)))
      lexicalItemExtractor.lexicalItemsFromSentence(sentence) shouldBe expected
    }

    "extract multi word expression with words in between" in {
      val startingWord = generate[Token.Word]
      val otherWords = genWords(1, 5)
      val secondWord = generate[Token.Word]
      val sentence = startingWord +: otherWords :+ secondWord

      doReturn(Set.empty)
        .when(mweSet)
        .getMwesStartingWith(anyObject)
      doReturn(Set(Seq(secondWord.lemma)))
        .when(mweSet)
        .getMwesStartingWith(startingWord.lemma)

      val expectedMultiWord = Seq(LexicalItem.MultiWordExpression(Seq(startingWord, secondWord)))
      val expectedOthers = otherWords.map(LexicalItem.SingleWord(_))
      val expected = expectedMultiWord ++ expectedOthers
      lexicalItemExtractor.lexicalItemsFromSentence(sentence).withEmptyContext shouldBe expected

    }

    "extract multi word expression with multiple MWEs for the first word" in {
      val startingWord = generate[Token.Word]
      val secondWord = generate[Token.Word]
      val sentence = Seq(startingWord, secondWord)
      val countMwes = generate(Gen.chooseNum(1, 5))
      val otherMwes = (1 to countMwes).map(_ => genWords(1, 5).map(_.lemma))
      val mwes = (otherMwes :+ Seq(secondWord.lemma)).toSet

      doReturn(mwes)
        .when(mweSet)
        .getMwesStartingWith(startingWord.lemma)

      val expected = Seq(LexicalItem.MultiWordExpression(Seq(startingWord, secondWord), Some(TextContext.SurroundingWords.Empty)))
      lexicalItemExtractor.lexicalItemsFromSentence(sentence) shouldBe expected
    }

    "not extract multi word expression with not matched MWE for the first word" in {
      val startingWord = generate[Token.Word]
      val secondWord = generate[Token.Word]
      val otherWord = generateSuchThat[Token.Word](_ != secondWord)
      val sentence = Seq(startingWord, otherWord)

      doReturn(Set.empty)
        .when(mweSet)
        .getMwesStartingWith(anyObject)
      doReturn(Set(Seq(secondWord.lemma)))
        .when(mweSet)
        .getMwesStartingWith(startingWord.lemma)

      val expected = Seq(LexicalItem.SingleWord(startingWord), LexicalItem.SingleWord(otherWord))
      lexicalItemExtractor.lexicalItemsFromSentence(sentence).withEmptyContext shouldBe expected
    }

    "not extract multi word expression with not fully matched MWE" in {
      val startingWord = generate[Token.Word]
      val secondWord = generate[Token.Word]
      val thirdWord = generateSuchThat[Token.Word](_ != secondWord)
      val sentence = Seq(startingWord, secondWord)

      doReturn(Set.empty)
        .when(mweSet)
        .getMwesStartingWith(anyObject)
      doReturn(Set(Seq(secondWord.lemma, thirdWord.lemma)))
        .when(mweSet)
        .getMwesStartingWith(startingWord.lemma)

      val expected = Seq(LexicalItem.SingleWord(startingWord), LexicalItem.SingleWord(secondWord))
      lexicalItemExtractor.lexicalItemsFromSentence(sentence).withEmptyContext shouldBe expected
    }

    "not extract multi word expression with punctuation in between" in {
      val startingWord = generate[Token.Word]
      val punctuation = generate[Token.Punctuation]
      val secondWord = generate[Token.Word]
      val sentence = Seq(startingWord, punctuation, secondWord)

      doReturn(Set.empty)
        .when(mweSet)
        .getMwesStartingWith(anyObject)
      doReturn(Set(Seq(secondWord.lemma)))
        .when(mweSet)
        .getMwesStartingWith(startingWord.lemma)

      val expected = Seq(LexicalItem.SingleWord(startingWord), LexicalItem.SingleWord(secondWord))
      lexicalItemExtractor.lexicalItemsFromSentence(sentence).withEmptyContext shouldBe expected

    }

    "not extract multi word expression not in order" in {
      val startingWord = generate[Token.Word]
      val secondWord = generate[Token.Word]
      val sentence = Seq(secondWord, startingWord)

      doReturn(Set.empty)
        .when(mweSet)
        .getMwesStartingWith(anyObject)
      doReturn(Set(Seq(secondWord.lemma)))
        .when(mweSet)
        .getMwesStartingWith(startingWord.lemma)

      val expected = Seq(LexicalItem.SingleWord(secondWord), LexicalItem.SingleWord(startingWord))
      lexicalItemExtractor.lexicalItemsFromSentence(sentence).withEmptyContext shouldBe expected

    }

    "extract single words with context" in {
      val words = genWords(2, 10)

      doReturn(Set.empty)
        .when(mweSet)
        .getMwesStartingWith(anyObject)

      val expected = words.map { word =>
        LexicalItem.SingleWord(word, Some(ContextExtractor(words, word)))
      }

      lexicalItemExtractor.lexicalItemsFromSentence(words) shouldBe expected
    }

    "extract mwe with context" in {
      val before = genWords(1, 5)
      val after = genWords(1, 5)
      val firstMweWord = generate[Token.Word]
      val secondMweWord = generate[Token.Word]
      val mwe = Seq(firstMweWord, secondMweWord)
      val sentence = before ++ mwe ++ after

      doReturn(Set.empty)
        .when(mweSet)
        .getMwesStartingWith(anyObject)
      doReturn(Set(Seq(secondMweWord.lemma)))
        .when(mweSet)
        .getMwesStartingWith(firstMweWord.lemma)

      val expectedMwe = LexicalItem.MultiWordExpression(mwe, Some(ContextExtractor(sentence, mwe)))

      val actualMwe = lexicalItemExtractor.lexicalItemsFromSentence(sentence).collect {
        case mwe: LexicalItem.MultiWordExpression =>
          mwe
      }.head

      actualMwe shouldBe expectedMwe
    }

  }

}

object LexicalItemExtractorImplSpec {

  implicit class RichSeqItems(items: Seq[LexicalItem]) {

    def withEmptyContext: Seq[LexicalItem] = items.map(_.setContext(None))

  }

}
