package ru.poplavkov.foreader

import java.io.{File, FileOutputStream}
import java.time.Instant

import cats.effect.{ExitCode, IO, IOApp, Resource, Sync}
import cats.implicits._
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import ru.poplavkov.foreader.ContextVectorsCalculator.LocalDir
import ru.poplavkov.foreader.text.impl.CoreNlpTokenExtractor
import ru.poplavkov.foreader.text.{Token, TokenExtractor}
import ru.poplavkov.foreader.vector.{MathVector, VectorsMap}
import ru.poplavkov.foreader.word2vec.VectorsExtractor

import scala.language.higherKinds

/**
  * Calculates context vectors for each word in the input corpus
  * using already existing vectors matrix
  *
  * @author mpoplavkov
  */
class ContextVectorsCalculator[F[_] : Sync] {

  private val id = Instant.now.toEpochMilli
  private val WorkDir: File = new File(s"$LocalDir/context_vectors_$id")
  private val SeparateFilesDir: File = childFile(WorkDir, "separate")
  WorkDir.mkdir()
  SeparateFilesDir.mkdir()

  def calculate(vectorsFile: File,
                corpus: File,
                contextLen: Int = 3,
                language: Language = Language.English): F[Unit] = {
    val tokenExtractor = new CoreNlpTokenExtractor[F](language)
    for {
      vectorsMap <- VectorsExtractor.extractVectors[F](vectorsFile.toPath)
      _ <- Sync[F].delay(println("Vectors extracted"))
      files = corpus.listFiles.toList
      _ <- files.traverse(calculateForOneFile(_, SeparateFilesDir, tokenExtractor, contextLen, vectorsMap, language))
      _ <- combineVectorFiles(SeparateFilesDir, childFile(WorkDir, "context_vectors.txt"))
    } yield ()

  }

  private def calculateForOneFile(file: File,
                                  targetDir: File,
                                  tokenExtractor: TokenExtractor[F],
                                  contextLen: Int,
                                  vectorsMap: VectorsMap,
                                  language: Language): F[Unit] = {
    val text = FileUtil.readFile(file.toPath)
    val outFile = childFile(targetDir, file.getName)
    val fileSizeMb = file.length.toFloat / 1024 / 1024
    for {
      _ <- Sync[F].delay(println(f"Start processing file of size $fileSizeMb%1.2fmb: `${file.getName}`"))

      tokens <- tokenExtractor.extract(text)

      words = tokens.collect {
        case Token.Word(_, _, lemma, _) => lemma
      }

      map <- Sync[F].delay(words.indices.map { wordInd =>
        val fromLeft = (wordInd - contextLen) max 0
        val toRight = (wordInd + contextLen) min (words.length - 1)
        val leftVectors = (fromLeft until wordInd).map(words.apply).flatMap(vectorsMap.getVector)
        val rightVectors = (toRight until wordInd by -1).map(words.apply).flatMap(vectorsMap.getVector)
        words(wordInd).toString -> VectorUtil.avgVector(vectorsMap.dimension, leftVectors ++ rightVectors)
      }.groupBy(_._1).mapValues(_.map(_._2)))

      _ <- Sync[F].delay(println(s"Vectors created. Flushing to the `${outFile.getAbsolutePath}`"))

      json = map.asJson
      _ <- writeToFile(outFile, json.spaces2)

      _ <- Sync[F].delay(println(s"Processed ${file.getName}"))
    } yield ()

  }

  private def combineVectorFiles(dir: File, outFile: File): F[Unit] = {
    val combinedMap = dir.listFiles.map { file =>
      val content = FileUtil.readFile(file.toPath)
      decode[Map[String, Seq[MathVector]]](content).right.get
    }.reduce(mergeMaps[String, Seq[MathVector]](_ ++ _))

    for {
      _ <- Sync[F].delay(println("Combining vector files"))
      _ <- writeToFile(outFile, combinedMap.asJson.spaces2)
      _ <- Sync[F].delay(println("Finished"))
    } yield ()
  }

  private def writeToFile(file: File, toWrite: String): F[Unit] =
    Resource.fromAutoCloseable(Sync[F].delay(new FileOutputStream(file))).use { os =>
      Sync[F].delay(os.write(toWrite.getBytes))
    }

  private def mergeMaps[K, V](f: (V, V) => V)(map1: Map[K, V], map2: Map[K, V]): Map[K, V] = {
    val combined = map1.map { case (k, v1) =>
      val newValue = map2.get(k).map(f(v1, _)).getOrElse(v1)
      k -> newValue
    }
    map2 ++ combined
  }

  private def childFile(dir: File, childName: String): File =
    dir.toPath.resolve(childName).toFile

}

object ContextVectorsCalculator extends IOApp {

  private val LocalDir = ".local"

  private val calculator = new ContextVectorsCalculator[IO]

  private val VectorsFile = new File(s"$LocalDir/vectors.txt")
  private val CorpusDir = new File(s"$LocalDir/corpus")

  override def run(args: List[String]): IO[ExitCode] =
    calculator.calculate(VectorsFile, CorpusDir).map(_ => ExitCode.Success)

}
