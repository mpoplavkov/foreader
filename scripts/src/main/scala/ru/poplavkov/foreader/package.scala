package ru.poplavkov

import java.io.File

import io.circe.KeyEncoder
import ru.poplavkov.foreader.Globals.DictionaryMeaningId
import ru.poplavkov.foreader.vector.MathVector

import scala.language.{higherKinds, implicitConversions}

package object foreader {

  val LocalDir = new File(".local")
  LocalDir.mkdir()

  type WordToVectorsMap = Map[WordWithPos, Seq[MathVector]]

  implicit val meaningIdKeyEncoder: KeyEncoder[DictionaryMeaningId] = identity

}
