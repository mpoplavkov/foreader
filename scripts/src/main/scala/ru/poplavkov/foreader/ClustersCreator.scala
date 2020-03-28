package ru.poplavkov.foreader

import java.io.File

import cats.effect.{ExitCode, IO, IOApp, Sync}
import cats.implicits._
import io.circe.generic.auto._
import io.circe.parser.decode
import io.circe.syntax._
import ru.poplavkov.foreader.CreateClusterError.{NoMeaningsInDictionary, TooFewUsageExamples}
import ru.poplavkov.foreader.dictionary.impl.WordNetDictionaryImpl
import ru.poplavkov.foreader.text.{LexicalItem, Token}
import ru.poplavkov.foreader.vector.MathVector

import scala.language.higherKinds

/**
  * @author mpoplavkov
  */
class ClustersCreator[F[_] : Sync] {

  private val dictionary = new WordNetDictionaryImpl[F]

  def createClusters(contextVectorsFile: File): F[Unit] = {
    val outFile = FileUtil.brotherFile(contextVectorsFile, "clusters.txt")
    val content = FileUtil.readFile(contextVectorsFile.toPath)
    val contextsByWord = decode[WordToVectorsMap](content).right.get
    val wordsCount = contextsByWord.size
    val onePercent = wordsCount / 100
    val createClusterResults: F[List[Either[CreateClusterError, (WordWithPos, Seq[MathVector])]]] =
      contextsByWord.toList.zipWithIndex
        .traverse { case ((wordPos@WordWithPos(word, pos), vectors), ind) =>
          val token = Token.Word(0, word, word, pos)
          val item = LexicalItem.SingleWord(token)

          for {
            entry <- dictionary.getDefinition(item).value
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
      _ <- info(s"Created clusters for ${wordToCentroidsMap.size} words. Flushing")
      _ <- FileUtil.writeToFile(outFile, wordToCentroidsMap.asJson.spaces2)
      _ <- info(s"Successfully wrote data to ${outFile.getAbsolutePath}")
    } yield ()

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

object ClustersCreator extends IOApp {

  val creator = new ClustersCreator[IO]
  val lastContextVectorsDir: File = new File(LocalDir)
    .listFiles
    .filter(_.getName.startsWith("context_vectors"))
    .maxBy(_.getName)
  val contextVectorsFile: File = FileUtil.childFile(lastContextVectorsDir, "context_vectors.txt")

  override def run(args: List[String]): IO[ExitCode] =
    creator.createClusters(contextVectorsFile).map(_ => ExitCode.Success)
}
