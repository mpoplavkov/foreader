package ru.poplavkov.foreader.text.impl

import ru.poplavkov.foreader.text.{Token, TokensFilter}

/**
  * @author mpoplavkov
  */
object PunctuationTokensFilter extends TokensFilter {

  override protected def innerFilter: PartialFunction[Token, Boolean] = {
    case _: Token.Punctuation => false
  }

}
