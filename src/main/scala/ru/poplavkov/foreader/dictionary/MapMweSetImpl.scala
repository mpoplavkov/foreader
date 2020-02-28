package ru.poplavkov.foreader.dictionary

import cats.Applicative
import cats.syntax.applicative._
import ru.poplavkov.foreader.Globals.WordStr

import scala.language.higherKinds

/**
  * @author mpoplavkov
  */
class MapMweSetImpl[F[_] : Applicative](map: Map[WordStr, Set[Seq[WordStr]]]) extends MweSet[F] {

  override def getMwesStartingWith(startWord: WordStr): F[Set[Seq[WordStr]]] =
    map.getOrElse(startWord, Set.empty).pure

}
