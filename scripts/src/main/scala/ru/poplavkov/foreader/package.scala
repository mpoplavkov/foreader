package ru.poplavkov

import cats.effect.Sync

import scala.language.higherKinds

package object foreader {

  val LocalDir = ".local"

  def info[F[_] : Sync](info: String): F[Unit] = Sync[F].delay(println(info))

}
