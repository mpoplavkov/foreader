package ru.poplavkov.foreader.text

/**
  * @author mpoplavkov
  */
trait ContextExtractor {

  /**
    * Extract context for the token(s) starts at `startIndex`
    * and ends with `endIndex`
    */
  def extractContext(tokens: Seq[Token], startIndex: Int, endIndex: Int): TextContext

  final def extractContext(tokens: Seq[Token], elementIndex: Int): TextContext =
    extractContext(tokens, elementIndex, elementIndex)

}
