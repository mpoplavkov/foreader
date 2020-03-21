package ru.poplavkov.foreader.text.logging

import ru.poplavkov.foreader.Logging
import ru.poplavkov.foreader.text.LexicalItem
import ru.poplavkov.foreader.text.filter.LexicalItemFilter

/**
  * @author mpoplavkov
  */
trait LoggedLexicalItemFilter extends LexicalItemFilter with Logging {
  abstract override def filter(lexicalItem: LexicalItem): Boolean =
    super.filter(lexicalItem).logged("filter") {
      case false => s"filtered item `$lexicalItem`"
    }
}
