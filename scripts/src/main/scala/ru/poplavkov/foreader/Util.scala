package ru.poplavkov.foreader

import java.io.File

import cats.effect.Sync
import io.circe.parser.decode
import io.circe.syntax._
import io.circe.{Decoder, Encoder}

import scala.language.higherKinds

/**
  * @author mpoplavkov
  */
object Util {

  def info[F[_] : Sync](info: String): F[Unit] = Sync[F].delay(println(info))

  def readJsonFile[F[_] : Sync, T: Decoder](file: File): F[T] = Sync[F].delay {
    val content = FileUtil.readFile(file.toPath)
    decode[T](content).right.get
  }

  def writeToFileJson[F[_] : Sync, T: Encoder](file: File, t: T, readable: Boolean = true): F[Unit] = {
    val json = t.asJson
    val str = if (readable) json.spaces2 else json.noSpaces
    FileUtil.writeToFile(file, str)
  }

}
