package ru.poplavkov.foreader.text.filter.impl

import ru.poplavkov.foreader.Globals.WordStr
import ru.poplavkov.foreader.text.LexicalItem
import ru.poplavkov.foreader.text.filter.LexicalItemFilter

/**
  * @author mpoplavkov
  */
class StopwordsLexicalItemFilter(stopwords: Set[Seq[WordStr]]) extends LexicalItemFilter {
  override def filter(lexicalItem: LexicalItem): Boolean = !stopwords(lexicalItem.lemmas)
}
