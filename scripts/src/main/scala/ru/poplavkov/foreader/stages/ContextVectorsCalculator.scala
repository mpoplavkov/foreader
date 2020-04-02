package ru.poplavkov.foreader.stages

import java.io.File

import cats.effect.Sync
import cats.instances.list._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import io.circe.generic.auto._
import ru.poplavkov.foreader.Util._
import ru.poplavkov.foreader._
import ru.poplavkov.foreader.text.{Token, TokenExtractor}
import ru.poplavkov.foreader.vector.VectorsMap

import scala.language.higherKinds

/**
  * Calculates context vectors for each word in the input corpus
  * using already existing vectors matrix
  *
  * @author mpoplavkov
  */
class ContextVectorsCalculator[F[_] : Sync](tokenExtractor: TokenExtractor[F],
                                            vectorsMap: VectorsMap,
                                            contextLen: Int) {

  def calculate(corpus: File, workDir: File): F[WordToVectorsMap] = {
    for {
      _ <- corpus.listFiles.toList.traverse(calculateForOneFile)
      map <- combineVectorFiles(workDir)
    } yield map

  }

  private def calculateForOneFile(file: File): F[WordToVectorsMap] = {
    val text = FileUtil.readFile(file.toPath)
    val fileSizeMb = file.length.toFloat / 1024 / 1024
    for {
      _ <- info(f"Start processing file of size $fileSizeMb%1.2fmb: `${file.getName}`")

      tokens <- tokenExtractor.extract(text)
      map <- findContextVectors(tokens, vectorsMap)
    } yield map

  }

  private def findContextVectors(tokens: Seq[Token],
                                 vectorsMap: VectorsMap): F[WordToVectorsMap] = Sync[F].delay {
    tokens.zipWithIndex
      .collect { case (Token.Word(_, _, lemma, pos), ind) =>
        WordWithPos(lemma, pos) -> contextVectorByIndex(tokens, ind, vectorsMap, contextLen)
      }
      .groupBy(_._1).mapValues(_.map(_._2))
  }

  private def combineVectorFiles(dir: File): F[WordToVectorsMap] = Sync[F].delay {
    dir.listFiles
      .map(readJsonFile[WordToVectorsMap])
      .reduce(CollectionUtil.mergeMaps(_, _)(_ ++ _))
  }

}
