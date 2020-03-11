package ru.poplavkov.foreader.text.empty

import ru.poplavkov.foreader.LexicalItemGroup
import ru.poplavkov.foreader.text.LexicalItemGroupFilter

/**
  * @author mpoplavkov
  */
object EmptyLexicalItemGroupFilter extends LexicalItemGroupFilter {
  override def filter(group: LexicalItemGroup): Boolean = true
}
