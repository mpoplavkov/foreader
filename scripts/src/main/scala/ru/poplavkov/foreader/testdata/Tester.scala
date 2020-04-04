package ru.poplavkov.foreader.testdata

import java.io.File

import cats.effect.{ExitCode, IO, IOApp}
import cats.instances.list._
import cats.syntax.applicative._
import cats.syntax.traverse._
import io.circe.generic.auto._
import ru.poplavkov.foreader.Globals.DictionaryMeaningId
import ru.poplavkov.foreader.dictionary.empty.EmptyMweSetImpl
import ru.poplavkov.foreader.dictionary.impl.WordNetDictionaryImpl
import ru.poplavkov.foreader.dictionary.{Dictionary, DictionaryEntry}
import ru.poplavkov.foreader.text.impl.{ContextExtractorImpl, LexicalItemExtractorImpl}
import ru.poplavkov.foreader.text.{LexicalItem, LexicalItemExtractor, Token}
import ru.poplavkov.foreader.vector.{MathVector, VectorsMap}
import ru.poplavkov.foreader.word2vec.VectorsExtractor
import ru.poplavkov.foreader.{LocalDir, _}

import scala.io.StdIn
import scala.util.Try

/**
  * @author mpoplavkov
  */
object Tester extends IOApp {

  private val isHuman = true

  private val vectorsFile = new File(s"$LocalDir/vectors.txt")
  private val meaningsToVectorsFile = new File(s"$LocalDir/clustered_meanings.json")
  private val testCasesFile = new File(s"$LocalDir/test_data.json")
  private val testCases = readJsonFile[Seq[TestCase]](testCasesFile)
  private val humanResultFile = new File(s"$LocalDir/result_human.json")
  private val dictionaryResultFile = new File(s"$LocalDir/result_dictionary.json")

  private val contextLen = 3
  private val contextExtractor = new ContextExtractorImpl(contextLen)
  private val mweSet = new EmptyMweSetImpl[IO]
  private val meaningToVectorMap = readJsonFile[Map[DictionaryMeaningId, MathVector]](meaningsToVectorsFile)
  private val lexicalItemExtractor = new LexicalItemExtractorImpl[IO](mweSet, contextExtractor)

  override def run(args: List[String]): IO[ExitCode] = {
    val outFile = if (isHuman) humanResultFile else dictionaryResultFile

    for {
      vectorsMap <- if (isHuman) VectorsMap.Empty.pure[IO] else VectorsExtractor.extractVectors[IO](vectorsFile.toPath)

      dictionary = new WordNetDictionaryImpl[IO](vectorsMap, meaningToVectorMap)

      answers <- if (isHuman) {
        handleTestCasesHuman(testCases)
      } else {
        handleTestCasesDictionary(testCases, lexicalItemExtractor, dictionary)
      }

      _ <- writeToFileJson[IO, Map[String, DictionaryMeaningId]](outFile, answers)
    } yield ExitCode.Success
  }

  private def handleTestCasesDictionary(testCases: Seq[TestCase],
                                        lexicalItemExtractor: LexicalItemExtractor[IO],
                                        dictionary: Dictionary[IO]): IO[Map[String, DictionaryMeaningId]] = {
    val idToMeaningOpt = testCases.toList.traverse { case TestCase(id, sentence, word, _) =>
      for {
        items <- lexicalItemExtractor.lexicalItemsFromTokens(sentence)
        targetItem = items.find {
          case LexicalItem.SingleWord(w, _) if w == word => true
          case _ => false
        }
        meaningOpt <- targetItem match {
          case Some(item) =>
            dictionary.getDefinition(item).map(_.meanings.head).value
          case None =>
            Option.empty[DictionaryEntry.Meaning].pure[IO]
        }
      } yield id -> meaningOpt
    }

    idToMeaningOpt.map(_.collect { case (id, Some(meaning)) => (id, meaning.id) }.toMap)
  }

  private def handleTestCasesHuman(testCases: Seq[TestCase]): IO[Map[String, DictionaryMeaningId]] = IO.delay {
    val (answers, _) = testCases.foldLeft((Map.empty[String, DictionaryMeaningId], false)) {
      case ((result, cancelled), nextCase) =>
        if (cancelled) {
          (result, cancelled)
        } else {
          handleTestCaseHuman(nextCase) match {
            case Right(Some(meaning)) => (result + (nextCase.id -> meaning.id), cancelled)
            case Right(None) => (result, cancelled)
            case Left(_) => (result, true)
          }
        }
    }
    answers
  }

  private def handleTestCaseHuman(testCase: TestCase): Either[Unit, Option[DictionaryEntry.Meaning]] = {
    val TestCase(_, sentence, word, meanings) = testCase
    val headPosition = sentence.head.position
    sentence.map {
      case word: Token.Word => word.copy(position = word.position - headPosition)
      case punct: Token.Punctuation => punct.copy(position = punct.position - headPosition)
    }
    val sentenceStr = sentence.foldLeft("") { case (str, token) =>
      val tokenStr = token match {
        case Token.Word(_, original, _, _) => original
        case Token.Punctuation(_, mark) => mark.value
      }
      val spaces = " " * (token.position - str.length)
      s"$str$spaces$tokenStr"
    }

    println(sentenceStr)
    println(" " * word.position + "^" * word.original.length)
    meanings.zipWithIndex.foreach { case (meaning, index) =>
      println(s"$index) ${meaning.definition}")
    }

    val readLine = StdIn.readLine()
    if (readLine == "cancel") {
      Left(())
    } else {
      val answer = Try {
        val meaningIndex = readLine.toInt
        meanings(meaningIndex)
      }.toOption

      if (answer.isEmpty) {
        println("Skipping...")
      }
      Right(answer)
    }
  }
}
