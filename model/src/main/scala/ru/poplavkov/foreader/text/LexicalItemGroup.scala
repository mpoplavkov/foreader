package ru.poplavkov.foreader.text

import ru.poplavkov.foreader.dictionary.DictionaryEntry

/**
  * Group of the same lexical items with the same part of speech
  * and lemma
  *
  * @author mpoplavkov
  */
case class LexicalItemGroup(items: Seq[LexicalItem],
                            level: LexicalItemLevel,
                            definition: DictionaryEntry) {

  // TODO: get rid of Exceptions here
  require(items.nonEmpty, "items should be non empty")

}
