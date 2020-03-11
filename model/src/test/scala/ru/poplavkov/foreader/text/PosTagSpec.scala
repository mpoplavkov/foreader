package ru.poplavkov.foreader.text

import ru.poplavkov.foreader.SpecBase

/**
  * @author mpoplavkov
  */
class PosTagSpec extends SpecBase {

  case class TestCase(tag: String, token: Option[String], expected: Either[PunctuationMark, PartOfSpeech])

  val cases: Seq[TestCase] = Seq(
    TestCase("NNS", None, Right(PartOfSpeech.Noun)),
    TestCase("VB", None, Right(PartOfSpeech.Verb)),
    TestCase("CC", None, Right(PartOfSpeech.Conjunction)),
    TestCase("UN", None, Right(PartOfSpeech.Interjection)),
    TestCase("CD", None, Right(PartOfSpeech.Numeral)),
    TestCase("IN", None, Right(PartOfSpeech.Preposition)),
    TestCase("JJ", None, Right(PartOfSpeech.Adjective)),
    TestCase("PRP$", None, Right(PartOfSpeech.Pronoun)),
    TestCase("WRB", None, Right(PartOfSpeech.Adverb)),
    TestCase(",", None, Left(PunctuationMark.Comma)),
    TestCase(".", Some("."), Left(PunctuationMark.Dot)),
    TestCase(".", Some("!"), Left(PunctuationMark.Exclamation)),
    TestCase(".", Some("?"), Left(PunctuationMark.Question)),
    TestCase(":", Some(":"), Left(PunctuationMark.Colon)),
    TestCase(":", Some(";"), Left(PunctuationMark.Semicolon)),
    TestCase(":", Some("..."), Left(PunctuationMark.Dots)),
    TestCase(":", Some("-"), Left(PunctuationMark.Hyphen)),
    TestCase("(", Some("["), Left(PunctuationMark.Parenthesis("["))),
    TestCase("''", Some("\'"), Left(PunctuationMark.Quote("\'"))),
    TestCase("POS", None, Right(PartOfSpeech.Other("POS"))),
    TestCase("OTHER", None, Right(PartOfSpeech.Other("OTHER")))
  )

  "PosTag" should {
    cases.foreach { case TestCase(tag, tokenOpt, expected) =>
      val token = tokenOpt.getOrElse("")
      val tokenDescrPart = if (tokenOpt.nonEmpty) s"token '$token' and " else ""
      s"convert ${tokenDescrPart}tag '$tag' to $expected" in {
        val posTag = new PosTag(token, tag)
        posTag.asPartOfSpeechOrPunc shouldBe expected
      }
    }
  }

}
