package ru.poplavkov.foreader

import ru.poplavkov.foreader.DictionaryEntry._

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

  case class Meaning(definition: String,
                     partOfSpeech: Option[PartOfSpeech],
                     examples: Seq[String],
                     synonyms: Seq[String])

}
