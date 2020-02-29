package ru.poplavkov.foreader.dictionary.impl

import cats.Applicative
import cats.syntax.applicative._
import ru.poplavkov.foreader.Globals.WordStr
import ru.poplavkov.foreader.dictionary.{MweMap, MweSet}

import scala.language.higherKinds

/**
  * @author mpoplavkov
  */
class MapMweSetImpl[F[_] : Applicative](map: MweMap) extends MweSet[F] {

  override def getMwesStartingWith(startWord: WordStr): F[Set[Seq[WordStr]]] =
    map.getOrElse(startWord, Set.empty).pure

}
