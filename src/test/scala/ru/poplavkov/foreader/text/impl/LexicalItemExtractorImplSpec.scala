package ru.poplavkov.foreader.text.impl

import org.scalacheck.Gen
import ru.poplavkov.foreader.Generators._
import ru.poplavkov.foreader.SpecBase.CovariantId
import ru.poplavkov.foreader.dictionary.MweSet
import ru.poplavkov.foreader.text.{LexicalItemExtractor, Token}
import ru.poplavkov.foreader.{LexicalItem, SpecBase}

/**
  * @author mpoplavkov
  */
class LexicalItemExtractorImplSpec extends SpecBase {

  val mweSet: MweSet[CovariantId] = mock[MweSet[CovariantId]]

  def lexicalItemExtractor: LexicalItemExtractor[CovariantId] = new LexicalItemExtractorImpl(mweSet)

  "LexicalItemExtractorImpl" should {

    "extract lexical item from one word" in {
      val word = generate[Token.Word]

      doReturn(Set.empty)
        .when(mweSet)
        .getMwesStartingWith(anyObject)

      val expected = Seq(LexicalItem.SingleWord(word))
      lexicalItemExtractor.lexicalItemsFromTokens(Seq(word)) shouldBe expected
    }

    "extract sequence of single words" in {
      val words = genWords(2, 10)

      doReturn(Set.empty)
        .when(mweSet)
        .getMwesStartingWith(anyObject)

      val expected = words.map(LexicalItem.SingleWord)
      lexicalItemExtractor.lexicalItemsFromTokens(words) shouldBe expected
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

      val expected = words.map(LexicalItem.SingleWord)
      lexicalItemExtractor.lexicalItemsFromTokens(sentence) shouldBe expected
    }

    "extract small multi word expression" in {
      val startingWord = generate[Token.Word]
      val secondWord = generate[Token.Word]
      val sentence = Seq(startingWord, secondWord)

      doReturn(Set(Seq(secondWord.lemma)))
        .when(mweSet)
        .getMwesStartingWith(startingWord.lemma)

      val expected = Seq(LexicalItem.MultiWordExpression(Seq(startingWord, secondWord)))
      lexicalItemExtractor.lexicalItemsFromTokens(sentence) shouldBe expected

    }

    "extract long multi word expression" in {
      val startingWord = generate[Token.Word]
      val otherWords = genWords(2, 10)
      val sentence = startingWord +: otherWords

      doReturn(Set(otherWords.map(_.lemma)))
        .when(mweSet)
        .getMwesStartingWith(startingWord.lemma)

      val expected = Seq(LexicalItem.MultiWordExpression(sentence))
      lexicalItemExtractor.lexicalItemsFromTokens(sentence) shouldBe expected
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
      val expectedOthers = otherWords.map(LexicalItem.SingleWord)
      val expected = expectedMultiWord ++ expectedOthers
      lexicalItemExtractor.lexicalItemsFromTokens(sentence) shouldBe expected

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

      val expected = Seq(LexicalItem.MultiWordExpression(Seq(startingWord, secondWord)))
      lexicalItemExtractor.lexicalItemsFromTokens(sentence) shouldBe expected
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
      lexicalItemExtractor.lexicalItemsFromTokens(sentence) shouldBe expected
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
      lexicalItemExtractor.lexicalItemsFromTokens(sentence) shouldBe expected
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
      lexicalItemExtractor.lexicalItemsFromTokens(sentence) shouldBe expected

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
      lexicalItemExtractor.lexicalItemsFromTokens(sentence) shouldBe expected

    }

  }

  private def genWords(min: Int, max: Int): Seq[Token.Word] = {
    val count = generate(Gen.chooseNum(min, max))
    (1 to count).map(_ => generate[Token.Word])
  }

}
