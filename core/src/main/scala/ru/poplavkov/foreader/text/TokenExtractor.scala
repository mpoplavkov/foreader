package ru.poplavkov.foreader.text

import ru.poplavkov.foreader.Token

import scala.language.higherKinds

/**
  * @author mpoplavkov
  */
trait TokenExtractor[F[_]] {

  /**
    * Extracts all tokens from the given string
    */
  def extract(text: String): F[Seq[Token]]

}
