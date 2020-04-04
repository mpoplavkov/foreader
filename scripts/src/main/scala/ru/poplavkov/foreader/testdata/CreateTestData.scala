package ru.poplavkov.foreader.testdata

import java.io.File

import ru.poplavkov.foreader.{Language, writeToFileJson, _}
import cats.effect.{ExitCode, IO, IOApp}
import io.circe.generic.auto._
import ru.poplavkov.foreader.Globals.DictionaryMeaningId
import ru.poplavkov.foreader.dictionary.impl.WordNetDictionaryImpl
import ru.poplavkov.foreader.text.impl.CoreNlpTokenExtractor
import ru.poplavkov.foreader.vector.MathVector
import ru.poplavkov.foreader.word2vec.VectorsExtractor

/**
  * @author mpoplavkov
  */
object CreateTestData extends IOApp {

  private val N = 10

  private val inputFile = new File(s"$LocalDir/text.txt")
  private val outFile = new File(s"$LocalDir/test_data.json")
  private val vectorsFile = new File(s"$LocalDir/vectors.txt")
  private val meaningsToVectorsFile = new File(s"$LocalDir/clustered_meanings.json")

  private val inputText = FileUtil.readFile(inputFile.toPath)
  private val meaningToVectorMap = readJsonFile[Map[DictionaryMeaningId, MathVector]](meaningsToVectorsFile)
  private val tokenExtractor = new CoreNlpTokenExtractor[IO](Language.English)

  override def run(args: List[String]): IO[ExitCode] = for {
    vectorsMap <- VectorsExtractor.extractVectors[IO](vectorsFile.toPath)
    dictionary = new WordNetDictionaryImpl[IO](vectorsMap, meaningToVectorMap)
    testDataCreator = new TestDataCreator[IO](tokenExtractor, dictionary, meaningToVectorMap)
    testCases <- testDataCreator.create(inputText, N)
    _ <- writeToFileJson[IO, Seq[TestCase]](outFile, testCases)
  } yield ExitCode.Success

}
