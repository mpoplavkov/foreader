package ru.poplavkov.foreader.text

import ru.poplavkov.foreader.Card

import scala.language.higherKinds

/**
  * @author mpoplavkov
  */
trait TextProcessingEngine[F[_]] {

  def process(text: TextRepresentation[F], filter: LexicalItemGroupFilter): F[Seq[Card]]

}
