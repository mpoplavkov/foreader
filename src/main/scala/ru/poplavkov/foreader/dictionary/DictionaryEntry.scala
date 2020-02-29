package ru.poplavkov.foreader.dictionary

import ru.poplavkov.foreader.dictionary.DictionaryEntry.Meaning
import ru.poplavkov.foreader.text.PartOfSpeech

/**
  * Information for a specific word or MWE extracted from the dictionary
  *
  * @author mpoplavkov
  */
case class DictionaryEntry(meanings: Seq[Meaning])

object DictionaryEntry {

  case class Meaning(definition: String,
                     partOfSpeech: Option[PartOfSpeech],
                     examples: Seq[String],
                     synonyms: Seq[String])

}
