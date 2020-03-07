package ru.poplavkov.foreader.text

import ru.poplavkov.foreader.LexicalItem

/**
  * @author mpoplavkov
  */
trait LexicalItemFilter {

  protected def innerFilter: PartialFunction[LexicalItem, Boolean]

  final def filter(lexicalItem: LexicalItem): Boolean =
    innerFilter.applyOrElse(lexicalItem, (_: LexicalItem) => true)

  final def filterItems(items: Seq[LexicalItem]): Seq[LexicalItem] =
    items.filter(filter)

}
