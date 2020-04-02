package ru.poplavkov.foreader.text

/**
  * @author mpoplavkov
  */
trait ContextExtractor {

  /**
    * Extracts context for the tokens starts at `startIndex`
    * and ends at `endIndex`
    */
  def extractContext(tokens: Seq[Token], startIndex: Int, endIndex: Int): TextContext

  final def extractContext(tokens: Seq[Token], elementIndex: Int): TextContext =
    extractContext(tokens, elementIndex, elementIndex)

  /**
    * Extracts context given reversed Seq of tokens preceded some
    * item and Seq of tokens followed that item
    *
    * @param beforeReversed preceded tokens
    * @param after          followed tokens
    */
  def extractContext(beforeReversed: Seq[Token], after: Seq[Token]): TextContext

}
