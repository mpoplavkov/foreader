package ru.poplavkov

import java.io.File

import cats.effect.Sync
import io.circe.parser.decode
import io.circe.syntax._
import io.circe.{Decoder, Encoder, KeyDecoder, KeyEncoder}
import ru.poplavkov.foreader.Globals.DictionaryMeaningId
import ru.poplavkov.foreader.text.impl.ContextExtractorImpl
import ru.poplavkov.foreader.text.{TextContext, Token}
import ru.poplavkov.foreader.vector.{MathVector, VectorsMap}
import com.softwaremill.tagging._

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

  private val contextExtractor = new ContextExtractorImpl(contextLen = 3)

  def contextVectorByIndex(tokens: Seq[Token],
                           index: Int,
                           vectorsMap: VectorsMap): MathVector = {
    val surroundingWords = contextExtractor.extractContext(tokens, index) match {
      case TextContext.Empty => Seq.empty
      case TextContext.SurroundingWords(before, after) => before ++ after
    }
    val vectors = surroundingWords.flatMap(vectorsMap.getVector)
    VectorUtil.avgVector(vectorsMap.dimension, vectors)
  }

  implicit val meaningIdKeyEncoder: KeyEncoder[DictionaryMeaningId] = identity
  implicit val meaningIdKeyDecoder: KeyDecoder[DictionaryMeaningId] = s => Some(s.taggedWith)
}
