package ru.poplavkov.foreader

import java.io.File
import java.time.Instant

import cats.effect.{ExitCode, IO, IOApp, Sync}
import cats.implicits._
import io.circe.generic.auto._
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
class ContextVectorsCalculator[F[_] : Sync](language: Language = Language.English) {

  private val tokenExtractor = new CoreNlpTokenExtractor[F](language)
  private val id = Instant.now.toEpochMilli
  private val WorkDir: File = new File(s"$LocalDir/context_vectors_$id")
  private val SeparateFilesDir: File = FileUtil.childFile(WorkDir, "separate")
  WorkDir.mkdir()
  SeparateFilesDir.mkdir()

  def calculate(vectorsFile: File, corpus: File): F[Unit] = {
    for {
      vectorsMap <- VectorsExtractor.extractVectors[F](vectorsFile.toPath)
      _ <- info("Vectors extracted")
      files = corpus.listFiles.toList
      _ <- files.traverse(calculateForOneFile(_, SeparateFilesDir, tokenExtractor, vectorsMap, language))
      _ <- combineVectorFiles(SeparateFilesDir, FileUtil.childFile(WorkDir, "context_vectors.txt"))
    } yield ()

  }

  private def calculateForOneFile(file: File,
                                  targetDir: File,
                                  tokenExtractor: TokenExtractor[F],
                                  vectorsMap: VectorsMap,
                                  language: Language): F[Unit] = {
    val text = FileUtil.readFile(file.toPath)
    val outFile = FileUtil.childFile(targetDir, file.getName)
    val fileSizeMb = file.length.toFloat / 1024 / 1024
    for {
      _ <- info(f"Start processing file of size $fileSizeMb%1.2fmb: `${file.getName}`")

      tokens <- tokenExtractor.extract(text)
      map <- findContextVectors(tokens, vectorsMap)

      _ <- info(s"Vectors created. Flushing to the `${outFile.getAbsolutePath}`")
      _ <- writeToFileJson(outFile, map)
      _ <- info(s"Processed ${file.getName}")
    } yield ()

  }

  private def findContextVectors(tokens: Seq[Token],
                                 vectorsMap: VectorsMap): F[WordToVectorsMap] = Sync[F].delay {
    tokens.zipWithIndex
      .collect { case (Token.Word(_, _, lemma, pos), ind) =>
        WordWithPos(lemma, pos) -> contextVectorByIndex(tokens, ind, vectorsMap)
      }
      .groupBy(_._1).mapValues(_.map(_._2))
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
  private val CorpusDir = new File(s"$LocalDir/my_corpus")

  override def run(args: List[String]): IO[ExitCode] =
    calculator.calculate(VectorsFile, CorpusDir).map(_ => ExitCode.Success)

}
