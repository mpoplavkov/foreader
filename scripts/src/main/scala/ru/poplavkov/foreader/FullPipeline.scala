package ru.poplavkov.foreader

import java.io.File
import java.time.Instant

import cats.effect.{ExitCode, IO, IOApp}
import ru.poplavkov.foreader.stages.{ClustersCreator, ContextVectorsCalculator}
import ru.poplavkov.foreader.text.impl.CoreNlpTokenExtractor
import ru.poplavkov.foreader.word2vec.VectorsExtractor
import ru.poplavkov.foreader.Util._
import ru.poplavkov.foreader.dictionary.impl.WordNetDictionaryImpl
import ru.poplavkov.foreader.vector.VectorsMap

/**
  * @author mpoplavkov
  */
object FullPipeline extends IOApp {

  private val id = Instant.now.toEpochMilli
  private val WorkDir: File = new File(s"$LocalDir/context_vectors_$id")
  private val SeparateFilesDir: File = FileUtil.childFile(WorkDir, "separate")
  private val VectorsFile = new File(s"$LocalDir/vectors.txt")
  private val CorpusDir = new File(s"$LocalDir/corpus")
  WorkDir.mkdir()
  SeparateFilesDir.mkdir()

  private val contextLen = 3
  private val tokenExtractor = new CoreNlpTokenExtractor[IO](Language.English)
  private val dictionary = new WordNetDictionaryImpl[IO](VectorsMap.Empty, Map.empty)
  private val clustersCreator = new ClustersCreator[IO](dictionary)

  override def run(args: List[String]): IO[ExitCode] = for {
    vectorsMap <- VectorsExtractor.extractVectors[IO](VectorsFile.toPath)
    calculator = new ContextVectorsCalculator[IO](tokenExtractor, vectorsMap, contextLen)
    _ <- info[IO]("Vectors extracted")
    wordToVectorsMap <- calculator.calculate(VectorsFile, CorpusDir)
    _ <- info[IO]("Context vectors calculated")

    wordToCentroidsMap <- clustersCreator.createClusters(wordToVectorsMap)
    _ <- info[IO](s"Created clusters for ${wordToCentroidsMap.size} words")


  } yield ExitCode.Success

}
