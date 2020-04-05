package ru.poplavkov.foreader

import java.io.File

import cats.effect.Sync
import io.circe.parser.decode
import io.circe.syntax._
import io.circe.{Decoder, Encoder}
import ru.poplavkov.foreader.text.impl.ContextExtractorImpl
import ru.poplavkov.foreader.text.{TextContext, Token}
import ru.poplavkov.foreader.vector.{MathVector, VectorsMap}

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

  def writeToFileJson[F[_] : Sync, T: Encoder](file: File, t: T): F[Unit] =
    FileUtil.writeToFile(file, t.asJson.spaces2)

  def contextVectorByIndex(tokens: Seq[Token],
                           index: Int,
                           vectorsMap: VectorsMap,
                           contextLen: Int): MathVector = {
    val contextExtractor = new ContextExtractorImpl(contextLen)
    val surroundingWords = contextExtractor.extractContext(tokens, index) match {
      case TextContext.Empty => Seq.empty
      case TextContext.SurroundingWords(before, after) => before ++ after
    }
    val vectors = surroundingWords.flatMap(vectorsMap.getVector)
    VectorUtil.avgVector(vectorsMap.dimension, vectors)
  }

}
