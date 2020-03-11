package ru.poplavkov.foreader.text.empty

import ru.poplavkov.foreader.LexicalItem
import ru.poplavkov.foreader.text.LexicalItemFilter

/**
  * @author mpoplavkov
  */
object EmptyLexicalItemFilter extends LexicalItemFilter {
  override def filter(lexicalItem: LexicalItem): Boolean = true
}
