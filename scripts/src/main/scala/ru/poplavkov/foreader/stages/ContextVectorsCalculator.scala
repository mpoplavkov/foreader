package ru.poplavkov.foreader.stages

import java.io.File

import cats.effect.Sync
import cats.instances.list._
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import io.circe.generic.auto._
import ru.poplavkov.foreader.Util._
import ru.poplavkov.foreader._
import ru.poplavkov.foreader.dictionary.Dictionary
import ru.poplavkov.foreader.stages.ContextVectorsCalculator._
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
                                            dictionary: Dictionary[F],
                                            contextLen: Int) {

  def calculate(corpus: File, dirForSeparateFiles: File): F[Seq[File]] = {
    val corpusFiles = corpus.listFiles

    for {
      _ <- corpusFiles.zipWithIndex.toList.traverse { case (file, index) =>
        val fileSizeMb = file.length.toFloat / 1024 / 1024
        for {
          _ <- info(f"${index + 1}/${corpusFiles.size} of size $fileSizeMb%1.2fmb: `${file.getName}`")
          _ <- calculateForOneFile(file, dirForSeparateFiles)
        } yield ()
      }
      allDirs = dirForSeparateFiles.listFiles.filter(_.isDirectory)
      files <- allDirs.toList.traverse { dir =>
        for {
          _ <- info(s"combining separate files from ${dir.getAbsolutePath}")
          vectorsMap <- combineVectorFiles(dir)
          out = FileUtil.childFile(dirForSeparateFiles, s"${dir.getName}_context_vectors.json")
          _ <- writeToFileJson(out, vectorsMap)
        } yield out
      }
    } yield files

  }

  private def calculateForOneFile(file: File, separateFilesDir: File): F[Unit] = {
    val text = FileUtil.readFile(file.toPath)
    for {
      tokens <- tokenExtractor.extract(text)
      contextVectors <- findContextVectors(tokens)

      // filter by existing in the dictionary
      filtered <- contextVectors.toList.traverse { case (wordPos, vectors) =>
        dictionary.getDefinition(wordPos.word, wordPos.pos).map(_ => (wordPos, vectors)).value
      }.map(_.flatten)

      _ <- filtered.groupBy(_._1.word.head.toLower).toList.traverse { case (startLetter, wordToVectors) =>
        if (AllowedStartsOfWord(startLetter)) {
          val outDir = FileUtil.childFile(separateFilesDir, startLetter.toString)
          outDir.mkdir()
          val outFile = FileUtil.childFile(outDir, s"${file.getName}.json")
          writeToFileJson(outFile, wordToVectors.toMap)
        } else {
          ().pure[F]
        }
      }
    } yield ()

  }

  private def findContextVectors(tokens: Seq[Token]): F[WordToVectorsMap] = Sync[F].delay {
    tokens.zipWithIndex
      .collect { case (Token.Word(_, _, lemma, pos), ind) =>
        WordWithPos(lemma, pos) -> contextVectorByIndex(tokens, ind, vectorsMap, contextLen)
      }
      .groupBy(_._1).mapValues(_.map(_._2))
  }

  private def combineVectorFiles(dir: File): F[WordToVectorsMap] =
    dir.listFiles.toList
      .traverse(readJsonFile[F, WordToVectorsMap])
      .map(_.reduce(CollectionUtil.mergeMaps(_, _)(_ ++ _)))

}

object ContextVectorsCalculator {

  private val AllowedStartsOfWord: Set[Char] = ('a' to 'z').toSet

}
