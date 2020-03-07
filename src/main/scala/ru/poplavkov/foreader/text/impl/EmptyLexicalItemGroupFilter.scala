package ru.poplavkov.foreader.text.impl

import ru.poplavkov.foreader.text.{LexicalItemGroup, LexicalItemGroupFilter}

/**
  * @author mpoplavkov
  */
object EmptyLexicalItemGroupFilter extends LexicalItemGroupFilter {
  override def filter(group: LexicalItemGroup): Boolean = true
}
