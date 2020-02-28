package ru.poplavkov.foreader.text.impl

import ru.poplavkov.foreader.text.TokensFilter
import ru.poplavkov.foreader.text.Token

/**
  * Checks if [[Token]] satisfies all `filters`
  *
  * @author mpoplavkov
  */
class CompositeTokensFilter(filters: Seq[TokensFilter]) extends TokensFilter {

  override protected def innerFilter: PartialFunction[Token, Boolean] = {
    case token => filters.forall(_.filter(token))
  }

}
