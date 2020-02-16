package ru.poplavkov.foreader.dictionary

import scala.language.higherKinds

/**
  * @author mpoplavkov
  */
trait MweSetFactory[F[_]] {

  def createMweSet(): F[MweSet[F]]

}
