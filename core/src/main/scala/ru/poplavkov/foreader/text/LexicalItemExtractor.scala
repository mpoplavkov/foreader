package ru.poplavkov.foreader.text

import scala.language.higherKinds

/**
  * @author mpoplavkov
  */
trait LexicalItemExtractor[F[_]] {

  def lexicalItemsFromSentences(tokensBySentences: Seq[Seq[Token]]): F[Seq[LexicalItem]]

  final def lexicalItemsFromSentence(sentenceTokens: Seq[Token]): F[Seq[LexicalItem]] =
    lexicalItemsFromSentences(Seq(sentenceTokens))


}
