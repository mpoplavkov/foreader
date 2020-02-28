package ru.poplavkov.foreader.text.impl

import ru.poplavkov.foreader.text.{Token, TokensFilter}

/**
  * @author mpoplavkov
  */
class PunctuationTokensFilter extends TokensFilter {

  override protected def innerFilter: PartialFunction[Token, Boolean] = {
    case _: Token.Punctuation => false
  }

}
