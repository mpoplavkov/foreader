package ru.poplavkov.foreader

import java.io.File

import cats.effect.Sync
import cats.instances.list._
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import io.circe.generic.auto._
import ru.poplavkov.foreader.Globals.DictionaryMeaningId
import ru.poplavkov.foreader.TestDataCreator._
import ru.poplavkov.foreader.dictionary.{Dictionary, DictionaryEntry}
import ru.poplavkov.foreader.text.{Token, TokenExtractor}
import ru.poplavkov.foreader.vector.MathVector

import scala.language.higherKinds

class TestDataCreator[F[_] : Sync](tokenExtractor: TokenExtractor[F],
                                   dictionary: Dictionary[F],
                                   meaningToVectorMap: Map[DictionaryMeaningId, MathVector]) {

  def create(inputFile: File, outFile: File, n: Int): F[Unit] = {
    val text = FileUtil.readFile(inputFile.toPath)
    for {
      tokens <- tokenExtractor.extract(text)
      cases <- createTestCases(tokens, n)
      _ <- writeToFileJson(outFile, cases)
    } yield ()
  }

  private def createTestCases(tokens: Seq[Token], n: Int, cases: Seq[TestCase] = Seq.empty): F[Seq[TestCase]] = {
    if (cases.size >= n) {
      cases.take(n).pure
    } else if (tokens.isEmpty) {
      cases.pure
    } else {
      val sentence = tokens.takeWhile {
        case Token.Punctuation(_, mark) => !mark.isEndOfSentence
        case _ => true
      }
      val rest = tokens.drop(sentence.length + 1)
      testCasesFromSentence(sentence).flatMap { testCases =>
        createTestCases(rest, n, cases ++ testCases)
      }
    }
  }

  private def testCasesFromSentence(sentence: Seq[Token]): F[Seq[TestCase]] =
    for {
      suitable <- extractSuitableWords(sentence)
    } yield {
      suitable.map { case (word, meanings) =>
        TestCase(sentence, word, meanings)
      }
    }

  private def extractSuitableWords(sentence: Seq[Token]): F[Seq[(Token.Word, Seq[DictionaryEntry.Meaning])]] =
    sentence.toList.traverse {
      case token@Token.Word(_, _, lemma, partOfSpeech) =>
        for {
          entry <- dictionary.getDefinition(lemma, partOfSpeech).value
          meanings = entry.toSeq.flatMap(_.meanings)
          meaningsInMap = meanings.map(_.id).filter(meaningToVectorMap.contains)
          _ <- if (meaningsInMap.size < meanings.size) {
            info(s"Not all meanings in map: ${meaningsInMap.size}/${meanings.size}")
          } else {
            ().pure[F]
          }
        } yield {
          if (meaningsInMap.nonEmpty) {
            Some((token, meanings))
          } else {
            None
          }
        }
    }.map(_.flatten)

}

object TestDataCreator {

  case class TestCase(sentence: Seq[Token], word: Token.Word, meanings: Seq[DictionaryEntry.Meaning])

}