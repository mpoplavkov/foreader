package ru.poplavkov.foreader.testdata

import cats.effect.{ExitCode, IO, IOApp}
import cats.instances.list._
import cats.syntax.applicative._
import cats.syntax.traverse._
import io.circe.generic.auto._
import ru.poplavkov.foreader.Globals.DictionaryMeaningId
import ru.poplavkov.foreader.Util._
import ru.poplavkov.foreader._
import ru.poplavkov.foreader.dictionary.empty.EmptyMweSetImpl
import ru.poplavkov.foreader.dictionary.impl.WordNetDictionaryImpl
import ru.poplavkov.foreader.dictionary.{Dictionary, DictionaryEntry}
import ru.poplavkov.foreader.text.impl.LexicalItemExtractorImpl
import ru.poplavkov.foreader.text.{LexicalItem, LexicalItemExtractor, Token}
import ru.poplavkov.foreader.vector.{MathVector, VectorsMap}
import ru.poplavkov.foreader.word2vec.VectorsExtractor

import scala.io.StdIn
import scala.util.Try

/**
  * @author mpoplavkov
  */
object Tester extends IOApp {

  private val isHuman = true

  private val vectorsFile = FileUtil.childFile(LocalDir, "vectors.txt")
  private val meaningsToVectorsFile = FileUtil.childFile(LocalDir, "clustered_meanings.json")
  private val testCasesFile = FileUtil.childFile(TestDataDir, "test_data.json")
  private val humanResultFile = FileUtil.childFile(TestDataDir, "result_human.json")
  private val dictionaryResultFile = FileUtil.childFile(TestDataDir, "result_dictionary.json")

  private val mweSet = new EmptyMweSetImpl[IO]
  private val lexicalItemExtractor = new LexicalItemExtractorImpl[IO](mweSet, commonContextExtractor)

  override def run(args: List[String]): IO[ExitCode] = {
    val outFile = if (isHuman) humanResultFile else dictionaryResultFile

    for {
      testCases <- readJsonFile[IO, Seq[TestCase]](testCasesFile)
      meaningToVectorMap <- readJsonFile[IO, Map[DictionaryMeaningId, MathVector]](meaningsToVectorsFile)

      vectorsMap <- if (isHuman) VectorsMap.Empty.pure[IO] else VectorsExtractor.extractVectors[IO](vectorsFile.toPath)

      dictionary = new WordNetDictionaryImpl[IO](vectorsMap, meaningToVectorMap)

      alreadyAnswered <- if (outFile.exists()) {
        readJsonFile[IO, Answers](outFile)
      } else {
        Map.empty[String, Option[DictionaryMeaningId]].pure[IO]
      }

      restTestCases = testCases.filterNot(test => alreadyAnswered.contains(test.id))

      answers <- if (isHuman) {
        handleTestCasesHuman(restTestCases)
      } else {
        handleTestCasesDictionary(restTestCases, lexicalItemExtractor, dictionary)
      }

      allAnswers = alreadyAnswered ++ answers
      _ <- writeToFileJson[IO, Answers](outFile, allAnswers)

      _ <- if (humanResultFile.exists && dictionaryResultFile.exists) {
        for {
          humanResult <- readJsonFile[IO, Answers](humanResultFile)
          dictResult <- readJsonFile[IO, Answers](dictionaryResultFile)
          _ <- compareResults(humanResult, dictResult, restTestCases)
        } yield ()
      } else {
        ().pure[IO]
      }
    } yield ExitCode.Success
  }

  private def compareResults(humanResult: Answers,
                             dictionaryResult: Answers,
                             cases: Seq[TestCase]): IO[Unit] = {
    val testIds = cases.map(_.id)
    val humanAnswered = humanResult.filterKeys(testIds.contains).collect { case (key, Some(id))  => (key, id) }
    val dictionaryAnswered = dictionaryResult.filterKeys(testIds.contains).collect { case (key, Some(id)) => (key, id) }

    val commonKeys = humanAnswered.keySet.intersect(dictionaryAnswered.keySet)
    val commonAnswers = commonKeys.filter { key =>
      humanAnswered(key) == dictionaryAnswered(key)
    }
    val differentAnswers = commonKeys.diff(commonAnswers)
    val correct = commonAnswers.map { testId =>
      val testCase = cases.find(_.id == testId).get
      val meaning = testCase.meanings.find(_.id == humanAnswered(testId)).get
      val quiz = sentenceToQuiz(testCase.sentence, testCase.word)
      s"""Correct:
         |$quiz
         |${meaning.definition}
         |""".stripMargin
    }.mkString("\n")
    val incorrect = differentAnswers.map { testId =>
      val testCase = cases.find(_.id == testId).get
      val humanMeaning = testCase.meanings.find(_.id == humanAnswered(testId)).get
      val dictMeaning = testCase.meanings.find(_.id == dictionaryAnswered(testId)).get
      val quiz = sentenceToQuiz(testCase.sentence, testCase.word)
      s"""Incorrect:
         |$quiz
         |human     : ${humanMeaning.definition}
         |dictionary: ${dictMeaning.definition}
         |""".stripMargin
    }.mkString("\n")

    for {
      _ <- info[IO](s"Correctly identified ${commonAnswers.size}/${commonKeys.size} meanings")
      _ <- info[IO](correct)
      _ <- info[IO](incorrect)
    } yield ()
  }

  private def handleTestCasesDictionary(testCases: Seq[TestCase],
                                        lexicalItemExtractor: LexicalItemExtractor[IO],
                                        dictionary: Dictionary[IO]): IO[Map[String, Option[DictionaryMeaningId]]] =
    testCases.toList.traverse { case TestCase(id, sentence, word, _) =>
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
      } yield id -> meaningOpt.map(_.id)
    }.map(_.toMap)

  private def handleTestCasesHuman(testCases: Seq[TestCase]): IO[Map[String, Option[DictionaryMeaningId]]] = IO.delay {
    val (answers, _) = testCases.foldLeft((Map.empty[String, Option[DictionaryMeaningId]], false)) {
      case ((result, cancelled), nextCase) =>
        if (cancelled) {
          (result, cancelled)
        } else {
          handleTestCaseHuman(nextCase) match {
            case Right(Some(meaning)) => (result + (nextCase.id -> Some(meaning.id)), cancelled)
            case Right(None) => (result + (nextCase.id -> None), cancelled)
            case Left(_) => (result, true)
          }
        }
    }
    answers
  }

  private def handleTestCaseHuman(testCase: TestCase): Either[Unit, Option[DictionaryEntry.Meaning]] = {
    val TestCase(_, sentence, word, meanings) = testCase

    println(sentenceToQuiz(sentence, word))

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

  private def sentenceToQuiz(sentence: Seq[Token], targetWord: Token.Word): String = {
    val headPosition = sentence.head.position
    val shifted = sentence.map {
      case word: Token.Word => word.copy(position = word.position - headPosition)
      case punct: Token.Punctuation => punct.copy(position = punct.position - headPosition)
    }
    val sentenceStr = shifted.foldLeft("") { case (str, token) =>
      val tokenStr = token match {
        case Token.Word(_, original, _, _) => original
        case Token.Punctuation(_, mark) => mark.value
      }
      val spaces = " " * (token.position - str.length)
      s"$str$spaces$tokenStr"
    }
    val wordStr = " " * {targetWord.position - headPosition} + "^" * targetWord.original.length

    s"$sentenceStr\n$wordStr"
  }
}
