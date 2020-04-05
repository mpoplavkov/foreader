package ru.poplavkov.foreader.testdata

import java.io.File

import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.applicative._
import io.circe.generic.auto._
import ru.poplavkov.foreader.Globals.DictionaryMeaningId
import ru.poplavkov.foreader.Util._
import ru.poplavkov.foreader._
import ru.poplavkov.foreader.dictionary.DictionaryEntry
import ru.poplavkov.foreader.dictionary.impl.WordNetDictionaryImpl
import ru.poplavkov.foreader.text.impl.CoreNlpTokenExtractor
import ru.poplavkov.foreader.vector.{MathVector, VectorsMap}

/**
  * @author mpoplavkov
  */
object CreateTestData extends IOApp {

  private val N = 10

  private val inputFile = FileUtil.childFile(TestDataDir, "text.txt")
  private val meaningsToVectorsFile = new File(s"$LocalDir/clustered_meanings.json")
  private val outFile = FileUtil.childFile(TestDataDir, "test_data.json")

  private val inputText = FileUtil.readFile(inputFile.toPath)
  private val tokenExtractor = new CoreNlpTokenExtractor[IO](Language.English)

  private val dictEntryFilter: DictionaryEntry => Boolean = _.meanings.size <= 3

  override def run(args: List[String]): IO[ExitCode] = for {
    meaningToVectorMap <- readJsonFile[IO, Map[DictionaryMeaningId, MathVector]](meaningsToVectorsFile)
    dictionary = new WordNetDictionaryImpl[IO](VectorsMap.Empty, Map.empty)
    testDataCreator = new TestDataCreator[IO](tokenExtractor, dictionary, meaningToVectorMap, dictEntryFilter)
    testCases <- testDataCreator.create(inputText, N)
    previousTestCases <- if (outFile.exists()) {
      readJsonFile[IO, Seq[TestCase]](outFile)
    } else {
      Seq.empty[TestCase].pure[IO]
    }
    allTestCases = (previousTestCases ++ testCases).groupBy(_.id).values.map(_.head)
    _ <- info[IO](s"All test cases count = ${allTestCases.size}")
    _ <- writeToFileJson[IO, Seq[TestCase]](outFile, allTestCases.toSeq)
  } yield ExitCode.Success

}
