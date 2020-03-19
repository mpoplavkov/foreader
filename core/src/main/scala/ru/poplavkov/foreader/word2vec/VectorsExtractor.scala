package ru.poplavkov.foreader.word2vec

import java.nio.file.Path

import cats.Applicative
import cats.syntax.applicative._
import com.softwaremill.tagging._
import ru.poplavkov.foreader.FileUtil._
import ru.poplavkov.foreader.Globals.WordStrTag
import ru.poplavkov.foreader.vector.{MathVector, VectorsMap}

import scala.language.higherKinds

/**
  * Extracts [[VectorsMap]] from file
  *
  * @author mpoplavkov
  */
object VectorsExtractor {

  def extractVectors[F[_] : Applicative](path: Path): F[VectorsMap] = {
    val map = readFile(path).split("\n").map { row =>
      val columns = row.split(" ")
      val (word, vector) = columns.splitAt(1)
      word.head.taggedWith[WordStrTag] -> MathVector(vector.map(_.toFloat).toSeq)
    }.toMap

    val dimension = map.values.headOption.map(_.dimension).getOrElse(0)
    new VectorsMap(dimension, map).pure
  }

}
