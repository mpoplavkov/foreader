package ru.poplavkov.foreader.text

/**
  * @author mpoplavkov
  */
trait LexicalItemGroupFilter {

  def filter(group: LexicalItemGroup): Boolean

  final def filterGroups(groups: Seq[LexicalItemGroup]): Seq[LexicalItemGroup] =
    groups.filter(filter)

}
