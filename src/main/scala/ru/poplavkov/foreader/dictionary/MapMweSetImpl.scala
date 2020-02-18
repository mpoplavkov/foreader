package ru.poplavkov.foreader.dictionary

import cats.Applicative
import cats.syntax.applicative._
import ru.poplavkov.foreader.Globals.Word

import scala.language.higherKinds

/**
  * @author mpoplavkov
  */
class MapMweSetImpl[F[_] : Applicative](map: Map[Word, Set[Seq[Word]]]) extends MweSet[F] {

  override def getMwesStartingWith(startWord: Word): F[Set[Seq[Word]]] =
    map.getOrElse(startWord, Set.empty).pure

}
