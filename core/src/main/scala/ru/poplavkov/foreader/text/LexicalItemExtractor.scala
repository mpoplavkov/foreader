package ru.poplavkov.foreader.text

import scala.language.higherKinds

/**
  * @author mpoplavkov
  */
trait LexicalItemExtractor[F[_]] {

  def lexicalItemsFromTokens(tokens: Seq[Token]): F[Seq[LexicalItem]]

}
