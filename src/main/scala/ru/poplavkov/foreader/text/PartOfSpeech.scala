package ru.poplavkov.foreader.text

import enumeratum._

import scala.collection.immutable

/**
  * @author mpoplavkov
  */
sealed trait PartOfSpeech extends EnumEntry

object PartOfSpeech extends Enum[PartOfSpeech] {

  override def values: immutable.IndexedSeq[PartOfSpeech] = findValues

  case object Noun extends PartOfSpeech

  case object Pronoun extends PartOfSpeech

  case object Verb extends PartOfSpeech

  case object Adjective extends PartOfSpeech

  case object Adverb extends PartOfSpeech

  case object Preposition extends PartOfSpeech

  case object Conjunction extends PartOfSpeech

  case object Interjection extends PartOfSpeech

  case object Numeral extends PartOfSpeech

  case object Other extends PartOfSpeech

  // A POS tag (or part-of-speech tag) is a special label assigned to each token (word)
  // in a text corpus to indicate the part of speech and often also other grammatical
  // categories such as tense, number (plural/singular), case etc
  def fromPosTag(tag: String): PartOfSpeech = tag match {
    case "NN" | "NNS" | "NNP" | "NNPS" => Noun
    case "VB" | "VBD" | "VBG" | "VBN" | "VBP" | "VBZ" => Verb
    case "CC" => Conjunction
    case "UN" => Interjection
    case "CD" => Numeral
    case "IN" => Preposition
    case "JJ" | "JJR" | "JJS" => Adjective
    case "PRP" | "PRP$" | "WP" => Pronoun
    case "RB" | "RBR" | "RBS" | "WRB" => Adverb
    case "DT" | "EX" | "FW" | "LS" | "MD" | "PDT" | "POS" | "RP" | "SYM" | "TO" | "WDT" | "WP$" => Other
    case _ => Other
  }

}
