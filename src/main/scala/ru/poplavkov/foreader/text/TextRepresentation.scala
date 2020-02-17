package ru.poplavkov.foreader.text

import scala.language.higherKinds

/**
  * Representation of an input text. Could be a String, File, Stream etc
  *
  * @author mpoplavkov
  */
trait TextRepresentation[F[_]] {

  /**
    * Extracts first `n` tokens from represented text
    *
    * @return extracted tokens and a new [[TextRepresentation]] to find the next token in
    */
  def nextTokens(n: Int): F[(Seq[Token], TextRepresentation[F])]

}
