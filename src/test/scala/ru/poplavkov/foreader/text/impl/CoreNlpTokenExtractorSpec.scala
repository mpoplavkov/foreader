package ru.poplavkov.foreader.text.impl

import cats.effect.IO
import ru.poplavkov.foreader.SpecBase
import ru.poplavkov.foreader.text.Token._
import ru.poplavkov.foreader.text._

/**
  * @author mpoplavkov
  */
class CoreNlpTokenExtractorSpec extends SpecBase {

  val tokenExtractor: TokenExtractor[IO] = new CoreNlpTokenExtractor[IO](Language.English)

  case class TestCase(description: String, text: String, expectedTokens: Token*)

  val cases: Seq[TestCase] = Seq(
    TestCase(
      description = "parse noun",
      text = "Noun",
      Word(0, "Noun", "noun", PartOfSpeech.Noun)
    ),
    TestCase(
      description = "parse noun with spaces",
      text = "  Noun  ",
      Word(2, "Noun", "noun", PartOfSpeech.Noun)
    ),
    TestCase(
      description = "parse simple sentence",
      text = "Apple is a noun",
      Word(0, "Apple", "apple", PartOfSpeech.Noun),
      Word(6, "is", "be", PartOfSpeech.Verb),
      Word(9, "a", "a", PartOfSpeech.Other("DT")),
      Word(11, "noun", "noun", PartOfSpeech.Noun)
    ),
    TestCase(
      description = "parse enumeration",
      text = "Apples, oranges, kiwis are fruits.",
      Word(0, "Apples", "apple", PartOfSpeech.Noun),
      Punctuation(6, PunctuationMark.Comma),
      Word(8, "oranges", "orange", PartOfSpeech.Noun),
      Punctuation(15, PunctuationMark.Comma),
      Word(17, "kiwis", "kiwi", PartOfSpeech.Noun),
      Word(23, "are", "be", PartOfSpeech.Verb),
      Word(27, "fruits", "fruit", PartOfSpeech.Noun),
      Punctuation(33, PunctuationMark.Dot)
    ),
    TestCase(
      description = "parse sentence with hyphen",
      text = "London - is the capital!",
      Word(0, "London", "london", PartOfSpeech.Noun),
      Punctuation(7, PunctuationMark.Hyphen),
      Word(9, "is", "be", PartOfSpeech.Verb),
      Word(12, "the", "the", PartOfSpeech.Other("DT")),
      Word(16, "capital", "capital", PartOfSpeech.Noun),
      Punctuation(23, PunctuationMark.Exclamation)
    ),
    TestCase(
      description = "parse '-' as a part of the word",
      text = "Well-being",
      Word(0, "Well-being", "well-being", PartOfSpeech.Noun)
    ),
    TestCase(
      description = "parse verb in the past",
      text = "Took off",
      Word(0, "Took", "take", PartOfSpeech.Verb),
      Word(5, "off", "off", PartOfSpeech.Other("RP"))
    ),
    TestCase(
      description = "correctly determine different parts of speech of the same word",
      text = "Orange is orange",
      Word(0, "Orange", "orange", PartOfSpeech.Noun),
      Word(7, "is", "be", PartOfSpeech.Verb),
      Word(10, "orange", "orange", PartOfSpeech.Adjective)
    ),
    TestCase(
      description = "parse a complex text",
      text = "“Oh, you foolish Alice!” she answered herself. “How can you learn lessons in here? " +
        "Why, there’s hardly room for you, and no room at all for any lesson-books!”",
      Punctuation(0, PunctuationMark.Quote("``")),
      Word(1, "Oh", "oh", PartOfSpeech.Other("UH")),
      Punctuation(3, PunctuationMark.Comma),
      Word(5, "you", "you", PartOfSpeech.Pronoun),
      Word(9, "foolish", "foolish", PartOfSpeech.Adjective),
      Word(17, "Alice", "alice", PartOfSpeech.Noun),
      Punctuation(22, PunctuationMark.Exclamation),
      Punctuation(23, PunctuationMark.Quote("''")),
      Word(25, "she", "she", PartOfSpeech.Pronoun),
      Word(29, "answered", "answer", PartOfSpeech.Verb),
      Word(38, "herself", "herself", PartOfSpeech.Pronoun),
      Punctuation(45, PunctuationMark.Dot),
      Punctuation(47, PunctuationMark.Quote("``")),
      Word(48, "How", "how", PartOfSpeech.Adverb),
      Word(52, "can", "can", PartOfSpeech.Other("MD")),
      Word(56, "you", "you", PartOfSpeech.Pronoun),
      Word(60, "learn", "learn", PartOfSpeech.Verb),
      Word(66, "lessons", "lesson", PartOfSpeech.Noun),
      Word(74, "in", "in", PartOfSpeech.Preposition),
      Word(77, "here", "here", PartOfSpeech.Adverb),
      Punctuation(81, PunctuationMark.Question),
      Word(83, "Why", "why", PartOfSpeech.Adverb),
      Punctuation(86, PunctuationMark.Comma),
      Word(88, "there", "there", PartOfSpeech.Other("EX")),
      Word(93, "’s", "be", PartOfSpeech.Verb),
      Word(96, "hardly", "hardly", PartOfSpeech.Adverb),
      Word(103, "room", "room", PartOfSpeech.Noun),
      Word(108, "for", "for", PartOfSpeech.Preposition),
      Word(112, "you", "you", PartOfSpeech.Pronoun),
      Punctuation(115, PunctuationMark.Comma),
      Word(117, "and", "and", PartOfSpeech.Conjunction),
      Word(121, "no", "no", PartOfSpeech.Other("DT")),
      Word(124, "room", "room", PartOfSpeech.Noun),
      Word(129, "at", "at", PartOfSpeech.Preposition),
      Word(132, "all", "all", PartOfSpeech.Other("DT")),
      Word(136, "for", "for", PartOfSpeech.Preposition),
      Word(140, "any", "any", PartOfSpeech.Other("DT")),
      Word(144, "lesson-books", "lesson-book", PartOfSpeech.Noun),
      Punctuation(156, PunctuationMark.Exclamation),
      Punctuation(157, PunctuationMark.Quote("''"))
    )
  )

  "CoreNlpTokenExtractor" should {
    cases.foreach { testCase =>
      testCase.description in {
        tokenExtractor.extract(testCase.text).unsafeRunSync() shouldBe testCase.expectedTokens
      }
    }

  }

}
