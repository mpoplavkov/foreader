package ru.poplavkov.foreader.text.impl

import cats.Applicative
import cats.data.OptionT
import ru.poplavkov.foreader.text.TextRepresentation

import scala.language.higherKinds

/**
  * @author mpoplavkov
  */
class EmptyTextRepresentation[F[_] : Applicative] extends TextRepresentation[F] {

  override def next(): OptionT[F, (String, TextRepresentation[F])] = OptionT.none

}
