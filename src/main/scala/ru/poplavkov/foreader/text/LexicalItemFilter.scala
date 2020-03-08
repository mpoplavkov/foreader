package ru.poplavkov.foreader.text

import ru.poplavkov.foreader.LexicalItem

/**
  * @author mpoplavkov
  */
trait LexicalItemFilter {

  def filter(lexicalItem: LexicalItem): Boolean

  final def filterItems(items: Seq[LexicalItem]): Seq[LexicalItem] =
    items.filter(filter)

}
