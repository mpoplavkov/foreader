package ru.poplavkov.foreader.text.impl

import ru.poplavkov.foreader.Globals.WordStr
import ru.poplavkov.foreader.LexicalItem
import ru.poplavkov.foreader.text.LexicalItemFilter

/**
  * @author mpoplavkov
  */
class StopwordsLexicalItemFilter(stopwords: Set[Seq[WordStr]]) extends LexicalItemFilter {
  override def filter(lexicalItem: LexicalItem): Boolean = !stopwords(lexicalItem.lemmas)
}
