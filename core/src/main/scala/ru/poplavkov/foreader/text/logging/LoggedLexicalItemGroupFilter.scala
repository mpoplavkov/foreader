package ru.poplavkov.foreader.text.logging

import ru.poplavkov.foreader.Logging
import ru.poplavkov.foreader.text.LexicalItemGroup
import ru.poplavkov.foreader.text.filter.LexicalItemGroupFilter

/**
  * @author mpoplavkov
  */
trait LoggedLexicalItemGroupFilter extends LexicalItemGroupFilter with Logging {

  abstract override def filter(group: LexicalItemGroup): Boolean =
    super.filter(group).logged("filter", Map("group" -> group)) {
      case false => s"filtered group with headOpt item = `${group.items.headOption}`"
    }

}
