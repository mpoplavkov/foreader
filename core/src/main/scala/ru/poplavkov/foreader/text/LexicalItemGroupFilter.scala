package ru.poplavkov.foreader.text

import ru.poplavkov.foreader.LexicalItemGroup

/**
  * @author mpoplavkov
  */
trait LexicalItemGroupFilter {

  def filter(group: LexicalItemGroup): Boolean

  final def filterGroups(groups: Seq[LexicalItemGroup]): Seq[LexicalItemGroup] =
    groups.filter(filter)

}
