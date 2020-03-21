package ru.poplavkov.foreader.text.logged

import ru.poplavkov.foreader.log.Logging
import ru.poplavkov.foreader.text.LexicalItem
import ru.poplavkov.foreader.text.filter.LexicalItemFilter

/**
  * @author mpoplavkov
  */
trait LoggedLexicalItemFilter extends LexicalItemFilter with Logging {
  abstract override def filter(lexicalItem: LexicalItem): Boolean =
    super.filter(lexicalItem).logged("filter", Map("lexicalItem" -> lexicalItem)) {
      case false => "filtered this item"
    }
}
