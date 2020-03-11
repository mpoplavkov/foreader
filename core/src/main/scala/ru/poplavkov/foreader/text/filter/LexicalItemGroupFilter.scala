package ru.poplavkov.foreader.text.filter

import ru.poplavkov.foreader.text.LexicalItemGroup

/**
  * @author mpoplavkov
  */
trait LexicalItemGroupFilter {

  def filter(group: LexicalItemGroup): Boolean

  final def filterGroups(groups: Seq[LexicalItemGroup]): Seq[LexicalItemGroup] =
    groups.filter(filter)

}
