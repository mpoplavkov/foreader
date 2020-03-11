package ru.poplavkov.foreader.text

import ru.poplavkov.foreader.text.PosTag._

/**
  * A POS tag (or part-of-speech tag) is a special label assigned to each token (word)
  * in a text corpus to indicate the part of speech and often also other grammatical
  * categories such as tense, number (plural/singular), case etc
  *
  * @author mpoplavkov
  */
class PosTag(token: String, tag: String) {

  def asPartOfSpeechOrPunc: Either[PunctuationMark, PartOfSpeech] = tag match {
    case "NN" | "NNS" | "NNP" | "NNPS" => Right(PartOfSpeech.Noun)
    case "VB" | "VBD" | "VBG" | "VBN" | "VBP" | "VBZ" => Right(PartOfSpeech.Verb)
    case "CC" => Right(PartOfSpeech.Conjunction)
    case "UN" => Right(PartOfSpeech.Interjection)
    case "CD" => Right(PartOfSpeech.Numeral)
    case "IN" => Right(PartOfSpeech.Preposition)
    case "JJ" | "JJR" | "JJS" => Right(PartOfSpeech.Adjective)
    case "PRP" | "PRP$" | "WP" => Right(PartOfSpeech.Pronoun)
    case "RB" | "RBR" | "RBS" | "WRB" => Right(PartOfSpeech.Adverb)
    case "," => Left(PunctuationMark.Comma)
    case "." => Left(sentenceFinalPunc(token))
    case ":" => Left(midSentencePunc(token))
    case "``" | "''" => Left(PunctuationMark.Quote(token))
    case "(" | ")" => Left(PunctuationMark.Parenthesis(token))
    case "DT" | "EX" | "FW" | "LS" | "MD" | "PDT" | "POS" | "RP" | "SYM" | "TO" | "WDT" | "WP$" => Right(PartOfSpeech.Other(tag))
    case _ => Right(PartOfSpeech.Other(tag))
  }

}

object PosTag {

  private def sentenceFinalPunc(token: String): PunctuationMark = token match {
    case "." => PunctuationMark.Dot
    case "!" => PunctuationMark.Exclamation
    case "?" => PunctuationMark.Question
    case _ => PunctuationMark.Other(token)
  }

  private def midSentencePunc(token: String): PunctuationMark = token match {
    case ":" => PunctuationMark.Colon
    case ";" => PunctuationMark.Semicolon
    case "..." => PunctuationMark.Dots
    case "-" => PunctuationMark.Hyphen
    case _ => PunctuationMark.Other(token)
  }

}
