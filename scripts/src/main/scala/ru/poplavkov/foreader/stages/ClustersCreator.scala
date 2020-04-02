package ru.poplavkov.foreader.stages

import cats.effect.Sync
import cats.instances.list._
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import ru.poplavkov.foreader.CreateClusterError.{NoMeaningsInDictionary, TooFewUsageExamples}
import ru.poplavkov.foreader.Util._
import ru.poplavkov.foreader._
import ru.poplavkov.foreader.dictionary.Dictionary
import ru.poplavkov.foreader.vector.MathVector

import scala.language.higherKinds

/**
  * @author mpoplavkov
  */
class ClustersCreator[F[_] : Sync](dictionary: Dictionary[F]) {

  def createClusters(contextsByWord: WordToVectorsMap): F[WordToVectorsMap] = {
    val wordsCount = contextsByWord.size
    val onePercent = wordsCount / 100

    val createClusterResults: F[List[Either[CreateClusterError, (WordWithPos, Seq[MathVector])]]] =
      contextsByWord.toList.zipWithIndex
        .traverse { case ((wordPos@WordWithPos(word, pos), vectors), ind) =>
          for {
            entry <- dictionary.getDefinition(word, pos).value
            meanings = entry.toSeq.flatMap(_.meanings)
            centroidsOrErr <- findCentroids(wordPos, meanings.size, vectors)
            _ <- if (ind % onePercent == 0) info(s"${ind / onePercent}%") else ().pure[F]
          } yield centroidsOrErr
        }

    for {
      results <- createClusterResults
      errors = results.collect { case Left(e) => e }
      _ <- logClusteringErrors(errors)
      wordToCentroidsMap = results.collect { case Right((word, centroids)) => word -> centroids }.toMap
    } yield wordToCentroidsMap

  }

  private def findCentroids(wordWithPos: WordWithPos,
                            meaningsCount: Int,
                            vectors: Seq[MathVector]): F[Either[CreateClusterError, (WordWithPos, Seq[MathVector])]] =
    Sync[F].delay {
      if (meaningsCount <= 0) {
        Left(NoMeaningsInDictionary(wordWithPos))
      } else {
        VectorUtil.kmeans(meaningsCount, vectors) match {
          case Some(centroids) => Right(wordWithPos -> centroids)
          case None => Left(TooFewUsageExamples(wordWithPos, vectors.size, meaningsCount))
        }
      }
    }

  private def logClusteringErrors(errors: Seq[CreateClusterError]): F[Unit] = {
    val noMeanings = errors.collect { case NoMeaningsInDictionary(word) => word }
    val tooFewExamples = errors.collect { case e: TooFewUsageExamples => e }
    for {
      _ <- info(s"Not found ${noMeanings.size} meanings in the dictionary: ${noMeanings.mkString(",")}")
      tooFewMsg = tooFewExamples.map { case TooFewUsageExamples(word, examples, definitions) =>
        s"$word(e=$examples,d=$definitions)"
      }.mkString(",")
      _ <- info(s"Too few examples of ${tooFewExamples.size} words: $tooFewMsg")
    } yield ()
  }
}
