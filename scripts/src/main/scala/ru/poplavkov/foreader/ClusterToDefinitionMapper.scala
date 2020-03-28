package ru.poplavkov.foreader

import java.io.File

import cats.effect.{ExitCode, IO, IOApp, Sync}
import cats.implicits._
import com.softwaremill.tagging._
import io.circe.generic.auto._
import io.circe.parser.decode
import io.circe.syntax._
import ru.poplavkov.foreader.dictionary.DictionaryEntry
import ru.poplavkov.foreader.dictionary.impl.WordNetDictionaryImpl
import ru.poplavkov.foreader.text.{LexicalItem, PartOfSpeech, Token}
import ru.poplavkov.foreader.vector.MathVector

import scala.language.higherKinds

/**
  * @author mpoplavkov
  */
class ClusterToDefinitionMapper[F[_] : Sync] {

  private val dictionary = new WordNetDictionaryImpl[F]

  def createClusters(contextVectorsFile: File): F[Unit] = {
    val outFile = FileUtil.childFile(contextVectorsFile.getParentFile, "clusters.txt")
    val content = FileUtil.readFile(contextVectorsFile.toPath)
    val contextsByWord = decode[Map[String, Seq[MathVector]]](content).right.get
    val wordsCount = contextsByWord.size
    val onePercent = wordsCount / 100
    val wordToCentroids: F[Map[String, Seq[MathVector]]] =
      contextsByWord.toList.zipWithIndex
        .traverse { case ((word, vectors), ind) =>
          val allDictionaryMeanings: F[List[DictionaryEntry.Meaning]] =
            PartOfSpeech.all.toList
              .traverse { pos =>
                val token = Token.Word(0, word.taggedWith, word.taggedWith, pos)
                val item = LexicalItem.SingleWord(token)
                dictionary.getDefinition(item).value
              }.map(_.flatten.flatMap(_.meanings))

          for {
            meanings <- allDictionaryMeanings
            meaningsCount = meanings.size
            centroids = if (meaningsCount > 0) VectorUtil.kmeans(meaningsCount, vectors) else Seq.empty
            _ <- if (ind % onePercent == 0) info(s"${ind / onePercent}%") else ().pure[F]
          } yield word -> centroids

        }.map(_.filter(_._2.nonEmpty).toMap)

    for {
      clustersMap <- wordToCentroids
      _ <- info("Clusters created. Flushing")
      _ <- FileUtil.writeToFile(outFile, clustersMap.asJson.spaces2)
      _ <- info(s"Successfully wrote data to ${outFile.getAbsolutePath}")
    } yield ()

  }
}

object ClusterToDefinitionMapper extends IOApp {

  val mapper = new ClusterToDefinitionMapper[IO]
  val lastContextVectorsDir: File = new File(LocalDir)
    .listFiles
    .filter(_.getName.startsWith("context_vectors"))
    .maxBy(_.getName)
  val contextVectorsFile: File = FileUtil.childFile(lastContextVectorsDir, "context_vectors.txt")

  override def run(args: List[String]): IO[ExitCode] =
    mapper.createClusters(contextVectorsFile).map(_ => ExitCode.Success)
}