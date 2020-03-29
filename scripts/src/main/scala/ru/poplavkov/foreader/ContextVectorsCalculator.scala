package ru.poplavkov.foreader

import java.io.File
import java.time.Instant

import cats.effect.{ExitCode, IO, IOApp, Sync}
import cats.implicits._
import io.circe.generic.auto._
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
  private val SeparateFilesDir: File = FileUtil.childFile(WorkDir, "separate")
  WorkDir.mkdir()
  SeparateFilesDir.mkdir()

  def calculate(vectorsFile: File,
                corpus: File,
                contextLen: Int = 3,
                language: Language = Language.English): F[Unit] = {
    val tokenExtractor = new CoreNlpTokenExtractor[F](language)
    for {
      vectorsMap <- VectorsExtractor.extractVectors[F](vectorsFile.toPath)
      _ <- info("Vectors extracted")
      files = corpus.listFiles.toList
      _ <- files.traverse(calculateForOneFile(_, SeparateFilesDir, tokenExtractor, contextLen, vectorsMap, language))
      _ <- combineVectorFiles(SeparateFilesDir, FileUtil.childFile(WorkDir, "context_vectors.txt"))
    } yield ()

  }

  private def calculateForOneFile(file: File,
                                  targetDir: File,
                                  tokenExtractor: TokenExtractor[F],
                                  contextLen: Int,
                                  vectorsMap: VectorsMap,
                                  language: Language): F[Unit] = {
    val text = FileUtil.readFile(file.toPath)
    val outFile = FileUtil.childFile(targetDir, file.getName)
    val fileSizeMb = file.length.toFloat / 1024 / 1024
    for {
      _ <- info(f"Start processing file of size $fileSizeMb%1.2fmb: `${file.getName}`")

      tokens <- tokenExtractor.extract(text)

      wordsWithPos = tokens.collect {
        case Token.Word(_, _, lemma, pos) => WordWithPos(lemma, pos)
      }

      map <- findContextVectors(wordsWithPos, vectorsMap, contextLen)

      _ <- info(s"Vectors created. Flushing to the `${outFile.getAbsolutePath}`")
      _ <- writeToFileJson(outFile, map)
      _ <- info(s"Processed ${file.getName}")
    } yield ()

  }

  private def findContextVectors(wordsWithPos: Seq[WordWithPos],
                                 vectorsMap: VectorsMap,
                                 contextLen: Int): F[WordToVectorsMap] = Sync[F].delay {
    def vectorsByIndices(indices: Seq[Int]): Seq[MathVector] =
      indices.map(wordsWithPos.apply).map(_.word).flatMap(vectorsMap.getVector)

    wordsWithPos.indices.map { wordInd =>
      val fromLeft = (wordInd - contextLen) max 0
      val toRight = (wordInd + contextLen) min (wordsWithPos.length - 1)
      val leftVectors = vectorsByIndices(fromLeft until wordInd)
      val rightVectors = vectorsByIndices(toRight until wordInd by -1)
      wordsWithPos(wordInd) -> VectorUtil.avgVector(vectorsMap.dimension, leftVectors ++ rightVectors)
    }.groupBy(_._1).mapValues(_.map(_._2))
  }

  private def combineVectorFiles(dir: File, outFile: File): F[Unit] = {
    val combinedMap = dir.listFiles
      .map(readJsonFile[WordToVectorsMap])
      .reduce(CollectionUtil.mergeMaps(_, _)(_ ++ _))

    for {
      _ <- info("Combining vector files")
      _ <- writeToFileJson(outFile, combinedMap)
      _ <- info("Finished")
    } yield ()
  }

}

object ContextVectorsCalculator extends IOApp {

  private val calculator = new ContextVectorsCalculator[IO]

  private val VectorsFile = new File(s"$LocalDir/vectors.txt")
  private val CorpusDir = new File(s"$LocalDir/corpus")

  override def run(args: List[String]): IO[ExitCode] =
    calculator.calculate(VectorsFile, CorpusDir).map(_ => ExitCode.Success)

}
