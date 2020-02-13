package ru.poplavkov.foreader.dictionary

/**
  * @author mpoplavkov
  */
trait MweSetFactory[F[_]] {

  def createMweSet(): F[MweSet[F]]

}
