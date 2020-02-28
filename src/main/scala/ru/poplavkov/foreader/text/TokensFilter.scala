package ru.poplavkov.foreader.text

/**
  * @author mpoplavkov
  */
trait TokensFilter {

  protected def innerFilter: PartialFunction[Token, Boolean]

  final def filter(token: Token): Boolean =
    innerFilter.applyOrElse(token, (_: Token) => true)

}
