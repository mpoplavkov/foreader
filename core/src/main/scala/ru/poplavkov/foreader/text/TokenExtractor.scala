package ru.poplavkov.foreader.text

import cats.Functor
import cats.syntax.functor._

import scala.language.higherKinds

/**
  * @author mpoplavkov
  */
trait TokenExtractor[F[_]] {

  /**
    * Extracts all tokens from the given string
    */
  final def extract(text: String)
                   (implicit functor: Functor[F]): F[Seq[Token]] =
    extractSentences(text).map(_.flatten)

  /**
    * Extracts all tokens from the given string grouped by sentences
    */
  def extractSentences(text: String): F[Seq[Seq[Token]]]

}
