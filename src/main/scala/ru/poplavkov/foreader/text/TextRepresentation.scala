package ru.poplavkov.foreader.text

import cats.data.OptionT

import scala.language.higherKinds

/**
  * Representation of an input text. Could be a String, File, Stream etc
  *
  * @author mpoplavkov
  */
trait TextRepresentation[F[_]] {

  /**
    * Extracts next string from the text
    *
    * @return extracted string and a new [[TextRepresentation]] to extract the next string from
    *         or `None` if the text is empty
    */
  def next(): OptionT[F, (String, TextRepresentation[F])]

}
