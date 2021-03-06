package ru.poplavkov.foreader.dictionary

import ru.poplavkov.foreader.Globals.DictionaryMeaningId
import ru.poplavkov.foreader.dictionary.DictionaryEntry._
import ru.poplavkov.foreader.text.PartOfSpeech

/**
  * Information for a specific word or MWE extracted from the dictionary
  *
  * @author mpoplavkov
  */
case class DictionaryEntry(meanings: Seq[Meaning]) {

  def isEmpty: Boolean = meanings.isEmpty

  def filterMeanings(p: Meaning => Boolean): DictionaryEntry =
    copy(meanings = meanings.filter(p))

}

object DictionaryEntry {

  case class Meaning(id: DictionaryMeaningId,
                     definition: String,
                     partOfSpeech: Option[PartOfSpeech],
                     examples: Seq[String],
                     synonyms: Seq[String])

}
