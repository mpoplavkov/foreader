package ru.poplavkov

import java.io.File

import cats.effect.Sync
import io.circe.parser.decode
import io.circe.syntax._
import io.circe.{Decoder, Encoder}
import ru.poplavkov.foreader.vector.MathVector

import scala.language.{higherKinds, implicitConversions}

package object foreader {

  val LocalDir = ".local"

  def info[F[_] : Sync](info: String): F[Unit] = Sync[F].delay(println(info))

  type WordToVectorsMap = Map[WordWithPos, Seq[MathVector]]

  def readJsonFile[T: Decoder](file: File): T = {
    val content = FileUtil.readFile(file.toPath)
    decode[T](content).right.get
  }

  def writeToFileJson[F[_] : Sync, T: Encoder](file: File, t: T): F[Unit] =
    FileUtil.writeToFile(file, t.asJson.spaces2)

}
