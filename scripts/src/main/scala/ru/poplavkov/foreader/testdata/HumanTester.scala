package ru.poplavkov.foreader.testdata

import java.io.File

import cats.effect.{ExitCode, IO, IOApp}
import io.circe.generic.auto._
import ru.poplavkov.foreader.Globals.DictionaryMeaningId
import ru.poplavkov.foreader.dictionary.DictionaryEntry
import ru.poplavkov.foreader.text.Token
import ru.poplavkov.foreader.{LocalDir, _}

import scala.io.StdIn
import scala.util.Try

/**
  * @author mpoplavkov
  */
object HumanTester extends IOApp {

  private val testCasesFile = new File(s"$LocalDir/test_data.json")
  private val testCases = readJsonFile[Seq[TestCase]](testCasesFile)
  private val resultFile = new File(s"$LocalDir/result_test_data.json")

  override def run(args: List[String]): IO[ExitCode] = {
    val (answers, _) = testCases.foldLeft((Map.empty[String, DictionaryMeaningId], false)) {
      case ((result, cancelled), nextCase) =>
        if (cancelled) {
          (result, cancelled)
        } else {
          handleTestCase(nextCase) match {
            case Right(Some(meaning)) => (result + (nextCase.id -> meaning.id), cancelled)
            case Right(None) => (result, cancelled)
            case Left(_) => (result, true)
          }
        }
    }

    writeToFileJson[IO, Map[String, DictionaryMeaningId]](resultFile, answers)
      .map(_ => ExitCode.Success)

  }


  private def handleTestCase(testCase: TestCase): Either[Unit, Option[DictionaryEntry.Meaning]] = {
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
