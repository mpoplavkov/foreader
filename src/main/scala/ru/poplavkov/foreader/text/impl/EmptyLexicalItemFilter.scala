package ru.poplavkov.foreader.text.impl

import ru.poplavkov.foreader.LexicalItem
import ru.poplavkov.foreader.text.LexicalItemFilter

/**
  * @author mpoplavkov
  */
object EmptyLexicalItemFilter extends LexicalItemFilter {
  override protected def innerFilter: PartialFunction[LexicalItem, Boolean] = {
    case _ => true
  }
}
