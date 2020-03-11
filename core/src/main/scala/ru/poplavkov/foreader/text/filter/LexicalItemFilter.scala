package ru.poplavkov.foreader.text.filter

import ru.poplavkov.foreader.text.LexicalItem

/**
  * @author mpoplavkov
  */
trait LexicalItemFilter {

  def filter(lexicalItem: LexicalItem): Boolean

  final def filterItems(items: Seq[LexicalItem]): Seq[LexicalItem] =
    items.filter(filter)

}
