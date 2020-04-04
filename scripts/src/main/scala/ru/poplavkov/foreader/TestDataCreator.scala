package ru.poplavkov.foreader

import java.io.File

import cats.effect.Sync
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.applicative._
import cats.syntax.traverse._
import cats.instances.list._
import ru.poplavkov.foreader.Globals.DictionaryMeaningId
import ru.poplavkov.foreader.dictionary.Dictionary
import ru.poplavkov.foreader.text.{Token, TokenExtractor}
import ru.poplavkov.foreader.vector.MathVector

import scala.language.higherKinds

class TestDataCreator[F[_] : Sync](tokenExtractor: TokenExtractor[F],
                                   dictionary: Dictionary[F],
                                   meaningToVectorMap: Map[DictionaryMeaningId, MathVector]) {

  def create(inputFile: File) = {
    val text = FileUtil.readFile(inputFile.toPath)
    for {
      tokens <- tokenExtractor.extract(text)
      sentences = tokens.

    } yield ()

  }


  private def d(tokens: Seq[Token]) = {
    val sentence = tokens.takeWhile {
      case Token.Punctuation(_, mark) => !mark.isEndOfSentence
      case _ => true
    }
    val rest = tokens.drop(sentence.length + 1)
  }

  private def processSentence(sentence: Seq[Token]) =
    for {
      suitable <- extractSuitableWords(sentence)
      _ <- if (suitable.isEmpty) {
        ().pure[F]
      } else {
        va
      }

    } yield ()

  private def extractSuitableWords(sentence: Seq[Token]): F[Seq[Token.Word]] =
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
            Some(token)
          } else {
            None
          }
        }
    }.map(_.flatten)

  private def processSuitableWord(word: Token.Word, sentence: Seq[Token]) = {
    val prefixSize = tokensToString(sentence.takeWhile(_ != word)).length + 2
    val prefix = " " * prefixSize
    val toPrint = s"${tokensToString(sentence)}\n$prefix^"
  }

  private def tokensToString(tokens: Seq[Token]): String =
    tokens.map {
      case Token.Word(_, original, _, _) => original
      case Token.Punctuation(_, mark) => mark.value
    }.mkString(" ")
}
