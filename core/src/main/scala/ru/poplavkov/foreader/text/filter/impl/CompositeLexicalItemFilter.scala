package ru.poplavkov.foreader.text.filter.impl

import ru.poplavkov.foreader.text.LexicalItem
import ru.poplavkov.foreader.text.filter.LexicalItemFilter

/**
  * Checks if [[LexicalItem]] satisfies all `filters`
  *
  * @author mpoplavkov
  */
class CompositeLexicalItemFilter(filters: Seq[LexicalItemFilter]) extends LexicalItemFilter {

  override def filter(lexicalItem: LexicalItem): Boolean =
    filters.forall(_.filter(lexicalItem))

}
