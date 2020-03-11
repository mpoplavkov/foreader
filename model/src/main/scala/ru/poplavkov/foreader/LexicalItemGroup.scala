package ru.poplavkov.foreader

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
  require(
    items.map(i => (i.lemmas, i.partsOfSpeech)).toSet.size == 1,
    "Lexical item group should contain items with the same lemma and part of speech"
  )
  require(items.nonEmpty, "items should be non empty")

}
