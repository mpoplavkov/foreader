package ru.poplavkov.foreader

import cats.ApplicativeError

import scala.language.higherKinds

/**
  * @author mpoplavkov
  */
trait ApplicativeErrorSupport[F[_]] {

  implicit protected def ae: ApplicativeError[F, Throwable]

}
