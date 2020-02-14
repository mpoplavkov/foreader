package ru.poplavkov.foreader

/**
  * Representation of an input text. Could be a String, File, Stream etc
  *
  * @author mpoplavkov
  */
sealed trait TextRepresentation[F[_]] {

  /**
    * Extract first token from represented text
    *
    * @return extracted token and a new [[TextRepresentation]] to find the next token in
    */
  def nextToken: F[(Token, TextRepresentation[F])]

}
