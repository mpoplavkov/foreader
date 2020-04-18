package ru.poplavkov.foreader.text

/**
  * @author mpoplavkov
  */
object ContextExtractor {

  /**
    * Extracts context for the `words` given the sentence, where these words
    * were extracted from
    */
  def apply(sentence: Seq[Token], words: Seq[Token.Word]): TextContext = {
    require(words.forall(sentence.contains), "Words should be a part of the sentence")
    val sentenceWords = sentence.collect { case word: Token.Word => word }
    val (before, afterWithItemWords) = sentenceWords.span(w => !words.contains(w))
    val after = afterWithItemWords.filterNot(words.contains)
    TextContext.SurroundingWords.fromTokens(before, after)
  }

  def apply(sentence: Seq[Token], word: Token.Word): TextContext =
    ContextExtractor(sentence, Seq(word))

}
