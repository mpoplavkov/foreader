package ru.poplavkov

import java.io.File

import cats.effect.Sync
import io.circe.parser.decode
import io.circe.syntax._
import io.circe.{Decoder, Encoder, KeyEncoder}
import ru.poplavkov.foreader.Globals.DictionaryMeaningId
import ru.poplavkov.foreader.vector.{MathVector, VectorsMap}

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

  def contextVectorByIndex(wordsWithPos: Seq[WordWithPos],
                           index: Int,
                           vectorsMap: VectorsMap,
                           contextLen: Int): MathVector = {
    val surroundingWords =
      CollectionUtil.surroundingElements(wordsWithPos.map(_.word), index, contextLen, contextLen)
    val vectors = surroundingWords.flatMap(vectorsMap.getVector)
    VectorUtil.avgVector(vectorsMap.dimension, vectors)
  }

  implicit val meaningIdKeyEncoder: KeyEncoder[DictionaryMeaningId] = identity
}
