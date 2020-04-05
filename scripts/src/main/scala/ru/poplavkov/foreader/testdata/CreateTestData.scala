package ru.poplavkov.foreader.testdata

import java.io.File

import cats.effect.{ExitCode, IO, IOApp}
import io.circe.generic.auto._
import ru.poplavkov.foreader.Globals.DictionaryMeaningId
import ru.poplavkov.foreader.Util._
import ru.poplavkov.foreader._
import ru.poplavkov.foreader.dictionary.impl.WordNetDictionaryImpl
import ru.poplavkov.foreader.text.impl.CoreNlpTokenExtractor
import ru.poplavkov.foreader.vector.{MathVector, VectorsMap}
import ru.poplavkov.foreader.word2vec.VectorsExtractor

/**
  * @author mpoplavkov
  */
object CreateTestData extends IOApp {

  private val N = 10

  private val inputFile = FileUtil.childFile(LocalDir, "text.txt")
  private val meaningsToVectorsFile = new File(s"$LocalDir/clustered_meanings.json")
  private val outFile = FileUtil.childFile(LocalDir, "test_data.json")

  private val inputText = FileUtil.readFile(inputFile.toPath)
  private val tokenExtractor = new CoreNlpTokenExtractor[IO](Language.English)

  override def run(args: List[String]): IO[ExitCode] = for {
    meaningToVectorMap <- readJsonFile[IO, Map[DictionaryMeaningId, MathVector]](meaningsToVectorsFile)
    dictionary = new WordNetDictionaryImpl[IO](VectorsMap.Empty, Map.empty)
    testDataCreator = new TestDataCreator[IO](tokenExtractor, dictionary, meaningToVectorMap)
    testCases <- testDataCreator.create(inputText, N)
    _ <- writeToFileJson[IO, Seq[TestCase]](outFile, testCases)
  } yield ExitCode.Success

}
