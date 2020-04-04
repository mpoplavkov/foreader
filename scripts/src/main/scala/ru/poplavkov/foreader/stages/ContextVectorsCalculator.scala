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
          out = FileUtil.childFile(dirForSeparateFiles, s"${dir.getName}_context_vectors.txt")
          _ <- writeToFileJson(out, vectorsMap)
        } yield out
      }
    } yield files

  }

  private def calculateForOneFile(file: File, separateFilesDir: File): F[Unit] = {
    val text = FileUtil.readFile(file.toPath)
    for {
      tokens <- tokenExtractor.extract(text)
      contextVectors <- findContextVectors(tokens, vectorsMap)
      filtered <- contextVectors.toList.traverse { case (wordPos, vectors) =>
        dictionary.getDefinition(wordPos.word, wordPos.pos).map(_ => (wordPos, vectors)).value
      }
      _ <- filtered.flatten.groupBy(_._1.word.head.toLower).toList.traverse { case (startLetter, wordToVectors) =>
        if (AllowedStartsOfWord(startLetter)) {
          val outDir = FileUtil.childFile(separateFilesDir, startLetter.toString)
          outDir.mkdir()
          val outFile = FileUtil.childFile(outDir, file.getName)
          writeToFileJson(outFile, wordToVectors.toMap)
        } else {
          ().pure[F]
        }
      }
    } yield ()

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
      .reduce[WordToVectorsMap] { case (map1, map2) =>
        CollectionUtil.mergeMaps(map1, map2) { (v1, v2) =>
          if (v1.size + v2.size > MaxArraySize) {
            println(s"WARN: Array size exceeds $MaxArraySize. Take only a $MaxArraySize elements...")
            val v2Rest = v2.take(MaxArraySize - v1.size)
            v1 ++ v2Rest
          } else {
            v1 ++ v2
          }
        }
      }
  }

}

object ContextVectorsCalculator {

  private val AllowedStartsOfWord: Set[Char] = ('a' to 'z').toSet

  // To not catch java.lang.OutOfMemoryError: Requested array size exceeds VM limit
  private val MaxArraySize = 1000000

}
