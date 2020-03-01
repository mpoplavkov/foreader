package ru.poplavkov.foreader.text

import ru.poplavkov.foreader.LexicalItem

/**
  * @author mpoplavkov
  */
trait LexicalItemExtractor[F[_]] {

  def lexicalItemsFromTokens(tokens: Seq[Token]): F[Seq[LexicalItem]]

}
