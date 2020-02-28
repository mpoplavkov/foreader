package ru.poplavkov.foreader.text.impl

import cats.effect.IO
import ru.poplavkov.foreader.SpecBase
import ru.poplavkov.foreader.SpecBase._
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
      Word(0, w"Noun", w"noun", PartOfSpeech.Noun)
    ),
    TestCase(
      description = "parse noun with spaces",
      text = "  Noun  ",
      Word(2, w"Noun", w"noun", PartOfSpeech.Noun)
    ),
    TestCase(
      description = "parse simple sentence",
      text = "Apple is a noun",
      Word(0, w"Apple", w"apple", PartOfSpeech.Noun),
      Word(6, w"is", w"be", PartOfSpeech.Verb),
      Word(9, w"a", w"a", PartOfSpeech.Other("DT")),
      Word(11, w"noun", w"noun", PartOfSpeech.Noun)
    ),
    TestCase(
      description = "parse enumeration",
      text = "Apples, oranges, kiwis are fruits.",
      Word(0, w"Apples", w"apple", PartOfSpeech.Noun),
      Punctuation(6, PunctuationMark.Comma),
      Word(8, w"oranges", w"orange", PartOfSpeech.Noun),
      Punctuation(15, PunctuationMark.Comma),
      Word(17, w"kiwis", w"kiwi", PartOfSpeech.Noun),
      Word(23, w"are", w"be", PartOfSpeech.Verb),
      Word(27, w"fruits", w"fruit", PartOfSpeech.Noun),
      Punctuation(33, PunctuationMark.Dot)
    ),
    TestCase(
      description = "parse sentence with hyphen",
      text = "London - is the capital!",
      Word(0, w"London", w"london", PartOfSpeech.Noun),
      Punctuation(7, PunctuationMark.Hyphen),
      Word(9, w"is", w"be", PartOfSpeech.Verb),
      Word(12, w"the", w"the", PartOfSpeech.Other("DT")),
      Word(16, w"capital", w"capital", PartOfSpeech.Noun),
      Punctuation(23, PunctuationMark.Exclamation)
    ),
    TestCase(
      description = "parse '-' as a part of the word",
      text = "Well-being",
      Word(0, w"Well-being", w"well-being", PartOfSpeech.Noun)
    ),
    TestCase(
      description = "parse verb in the past",
      text = "Took off",
      Word(0, w"Took", w"take", PartOfSpeech.Verb),
      Word(5, w"off", w"off", PartOfSpeech.Other("RP"))
    ),
    TestCase(
      description = "correctly determine different parts of speech of the same word",
      text = "Orange is orange",
      Word(0, w"Orange", w"orange", PartOfSpeech.Noun),
      Word(7, w"is", w"be", PartOfSpeech.Verb),
      Word(10, w"orange", w"orange", PartOfSpeech.Adjective)
    ),
    TestCase(
      description = "parse a complex text",
      text = "“Oh, you foolish Alice!” she answered herself. “How can you learn lessons in here? " +
        "Why, there’s hardly room for you, and no room at all for any lesson-books!”",
      Punctuation(0, PunctuationMark.Quote("``")),
      Word(1, w"Oh", w"oh", PartOfSpeech.Other("UH")),
      Punctuation(3, PunctuationMark.Comma),
      Word(5, w"you", w"you", PartOfSpeech.Pronoun),
      Word(9, w"foolish", w"foolish", PartOfSpeech.Adjective),
      Word(17, w"Alice", w"alice", PartOfSpeech.Noun),
      Punctuation(22, PunctuationMark.Exclamation),
      Punctuation(23, PunctuationMark.Quote("''")),
      Word(25, w"she", w"she", PartOfSpeech.Pronoun),
      Word(29, w"answered", w"answer", PartOfSpeech.Verb),
      Word(38, w"herself", w"herself", PartOfSpeech.Pronoun),
      Punctuation(45, PunctuationMark.Dot),
      Punctuation(47, PunctuationMark.Quote("``")),
      Word(48, w"How", w"how", PartOfSpeech.Adverb),
      Word(52, w"can", w"can", PartOfSpeech.Other("MD")),
      Word(56, w"you", w"you", PartOfSpeech.Pronoun),
      Word(60, w"learn", w"learn", PartOfSpeech.Verb),
      Word(66, w"lessons", w"lesson", PartOfSpeech.Noun),
      Word(74, w"in", w"in", PartOfSpeech.Preposition),
      Word(77, w"here", w"here", PartOfSpeech.Adverb),
      Punctuation(81, PunctuationMark.Question),
      Word(83, w"Why", w"why", PartOfSpeech.Adverb),
      Punctuation(86, PunctuationMark.Comma),
      Word(88, w"there", w"there", PartOfSpeech.Other("EX")),
      Word(93, w"’s", w"be", PartOfSpeech.Verb),
      Word(96, w"hardly", w"hardly", PartOfSpeech.Adverb),
      Word(103, w"room", w"room", PartOfSpeech.Noun),
      Word(108, w"for", w"for", PartOfSpeech.Preposition),
      Word(112, w"you", w"you", PartOfSpeech.Pronoun),
      Punctuation(115, PunctuationMark.Comma),
      Word(117, w"and", w"and", PartOfSpeech.Conjunction),
      Word(121, w"no", w"no", PartOfSpeech.Other("DT")),
      Word(124, w"room", w"room", PartOfSpeech.Noun),
      Word(129, w"at", w"at", PartOfSpeech.Preposition),
      Word(132, w"all", w"all", PartOfSpeech.Other("DT")),
      Word(136, w"for", w"for", PartOfSpeech.Preposition),
      Word(140, w"any", w"any", PartOfSpeech.Other("DT")),
      Word(144, w"lesson-books", w"lesson-book", PartOfSpeech.Noun),
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
