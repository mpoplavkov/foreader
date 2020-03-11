package ru.poplavkov.foreader.text.filter.empty

import ru.poplavkov.foreader.text.LexicalItemGroup
import ru.poplavkov.foreader.text.filter.LexicalItemGroupFilter

/**
  * @author mpoplavkov
  */
object EmptyLexicalItemGroupFilter extends LexicalItemGroupFilter {
  override def filter(group: LexicalItemGroup): Boolean = true
}
