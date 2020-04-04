package ru.poplavkov.foreader.dictionary.empty

import cats.Applicative
import cats.syntax.applicative._
import ru.poplavkov.foreader.Globals.WordStr
import ru.poplavkov.foreader.dictionary.MweSet

import scala.language.higherKinds

/**
  * @author mpoplavkov
  */
class EmptyMweSetImpl[F[_] : Applicative] extends MweSet[F] {

  override def getMwesStartingWith(startWord: WordStr): F[Set[Seq[WordStr]]] =
    Set.empty[Seq[WordStr]].pure[F]
}
