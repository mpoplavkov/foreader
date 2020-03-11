package ru.poplavkov.foreader.text.filter.empty

import ru.poplavkov.foreader.text.LexicalItem
import ru.poplavkov.foreader.text.filter.LexicalItemFilter

/**
  * @author mpoplavkov
  */
object EmptyLexicalItemFilter extends LexicalItemFilter {
  override def filter(lexicalItem: LexicalItem): Boolean = true
}
