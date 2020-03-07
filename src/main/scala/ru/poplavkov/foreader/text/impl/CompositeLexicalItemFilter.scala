package ru.poplavkov.foreader.text.impl

import ru.poplavkov.foreader.LexicalItem
import ru.poplavkov.foreader.text.{LexicalItemFilter, Token}

/**
  * Checks if [[Token]] satisfies all `filters`
  *
  * @author mpoplavkov
  */
class CompositeLexicalItemFilter(filters: Seq[LexicalItemFilter]) extends LexicalItemFilter {

  override protected def innerFilter: PartialFunction[LexicalItem, Boolean] = {
    case token => filters.forall(_.filter(token))
  }

}
