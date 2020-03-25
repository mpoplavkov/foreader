package ru.poplavkov.foreader

import java.io.{File, FileOutputStream}
import java.time.Instant

import cats.effect.{ExitCode, IO, IOApp, Resource, Sync}
import cats.implicits._
import io.circe.generic.auto._
import io.circe.syntax._
import ru.poplavkov.foreader.ContextVectorsCalculator.LocalDir
import ru.poplavkov.foreader.text.impl.CoreNlpTokenExtractor
import ru.poplavkov.foreader.text.{Token, TokenExtractor}
import ru.poplavkov.foreader.vector.VectorsMap
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
  WorkDir.mkdir()

  def calculate(vectorsFile: File,
                corpus: File,
                contextLen: Int = 3,
                language: Language = Language.English): F[Unit] = {
    val tokenExtractor = new CoreNlpTokenExtractor[F](language)
    for {
      vectorsMap <- VectorsExtractor.extractVectors[F](vectorsFile.toPath)
      _ <- Sync[F].delay(println("Vectors extracted"))
      files = corpus.listFiles.toList
      _ <- files.traverse(calculateForOneFile(_, tokenExtractor, contextLen, vectorsMap, language))
    } yield ()

  }

  private def calculateForOneFile(file: File,
                                  tokenExtractor: TokenExtractor[F],
                                  contextLen: Int,
                                  vectorsMap: VectorsMap,
                                  language: Language): F[Unit] = {
    val text = FileUtil.readFile(file.toPath)
    val outFile = WorkDir.toPath.resolve(file.getName).toFile
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
      resource = Resource.fromAutoCloseable(Sync[F].delay {
        new FileOutputStream(outFile)
      })
      _ <- resource.use(os => Sync[F].delay(os.write(json.spaces2.getBytes)))

      _ <- Sync[F].delay(println(s"Processed ${file.getName}"))
    } yield ()

  }

}

object ContextVectorsCalculator extends IOApp {

  private val LocalDir = ".local"

  private val calculator = new ContextVectorsCalculator[IO]

  private val VectorsFile = new File(s"$LocalDir/vectors.txt")
  private val CorpusDir = new File(s"$LocalDir/corpus")

  override def run(args: List[String]): IO[ExitCode] =
    calculator.calculate(VectorsFile, CorpusDir).map(_ => ExitCode.Success)

}
