package ru.poplavkov

import cats.effect.Sync
import ru.poplavkov.foreader.Globals.WordStr
import ru.poplavkov.foreader.text.PartOfSpeech
import ru.poplavkov.foreader.vector.MathVector

import scala.language.{higherKinds, implicitConversions}

package object foreader {

  val LocalDir = ".local"

  def info[F[_] : Sync](info: String): F[Unit] = Sync[F].delay(println(info))

  type WordToVectorsMap = Map[(String, PartOfSpeech), Seq[MathVector]]

  implicit def wordStr2String(wordStr: WordStr): String = wordStr.toString

}
