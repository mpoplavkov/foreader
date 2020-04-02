package ru.poplavkov.foreader.text

/**
  * @author mpoplavkov
  */
trait ContextExtractor {

  /**
    * Extracts context for the tokens starting at `startIndex`
    * and ending at `endIndex`
    */
  def extractContext(tokens: Seq[Token], startIndex: Int, endIndex: Int): TextContext

  final def extractContext(tokens: Seq[Token], elementIndex: Int): TextContext =
    extractContext(tokens, elementIndex, elementIndex)

  /**
    * Extracts context using tokens in reverse order preceding an
    * item and tokens following this item
    *
    * @param beforeReversed preceded tokens
    * @param after          followed tokens
    */
  def extractContext(beforeReversed: Seq[Token], after: Seq[Token]): TextContext

}
