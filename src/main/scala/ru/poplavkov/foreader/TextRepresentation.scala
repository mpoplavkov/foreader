package ru.poplavkov.foreader

/**
  * Representation of an input text. Could be a String, File, Stream etc
  * Main purpose of the class is to extract lexical items from the input text
  *
  * @author mpoplavkov
  */
sealed trait TextRepresentation[F[_]] {

  /**
    * Extract first [[LexicalItem]] from represented text
    *
    * @return extracted lexical item and a new [[TextRepresentation]] to find the next item in
    */
  def nextLexicalItem: F[(LexicalItem, TextRepresentation[F])]

}
