package ru.poplavkov.foreader.text.impl

import ru.poplavkov.foreader.Globals.WordStr
import ru.poplavkov.foreader.text.{Token, TokensFilter}

/**
  * @author mpoplavkov
  */
class StopwordsTokenFilter(stopwords: Set[WordStr]) extends TokensFilter {

  override protected def innerFilter: PartialFunction[Token, Boolean] = {
    case Token.Word(_, _, lemma, _) => !stopwords(lemma)
  }

}
