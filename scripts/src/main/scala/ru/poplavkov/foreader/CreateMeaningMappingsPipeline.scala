package ru.poplavkov.foreader

import java.io.File
import java.time.Instant

import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.traverse._
import cats.instances.list._
import io.circe.generic.auto._
import ru.poplavkov.foreader.Globals.DictionaryMeaningId
import ru.poplavkov.foreader.Util._
import ru.poplavkov.foreader.dictionary.impl.WordNetDictionaryImpl
import ru.poplavkov.foreader.stages.{ClusterToDefinitionMapper, ClustersCreator, ContextVectorsCalculator}
import ru.poplavkov.foreader.text.impl.CoreNlpTokenExtractor
import ru.poplavkov.foreader.vector.{MathVector, VectorsMap}
import ru.poplavkov.foreader.word2vec.VectorsExtractor

/**
  * @author mpoplavkov
  */
object CreateMeaningMappingsPipeline extends IOApp {

  private val corpusName = "corpus"
  private val id = Instant.now.toEpochMilli
  private val workDir: File = FileUtil.childFile(LocalDir, s"context_vectors_$id")
  private val separateFilesDir: File = FileUtil.childFile(workDir, "separate")
  workDir.mkdir()
  separateFilesDir.mkdir()
  private val vectorsFile = FileUtil.childFile(LocalDir, "vectors.txt")
  private val corpusDir = FileUtil.childFile(LocalDir, corpusName)
  private val outFile = FileUtil.childFile(workDir, "clustered_meanings.json")

  private val contextLen = 3
  private val tokenExtractor = new CoreNlpTokenExtractor[IO](Language.English)
  private val dictionary = new WordNetDictionaryImpl[IO](VectorsMap.Empty, Map.empty)
  private val clustersCreator = new ClustersCreator[IO](dictionary)

  override def run(args: List[String]): IO[ExitCode] = for {
    vectorsMap <- VectorsExtractor.extractVectors[IO](vectorsFile.toPath)
    _ <- info[IO]("Vectors extracted")

    calculator = new ContextVectorsCalculator[IO](tokenExtractor, vectorsMap, dictionary, contextLen)
    mapper = new ClusterToDefinitionMapper[IO](dictionary, tokenExtractor, vectorsMap, contextLen)

    vectorsFiles <- calculator.calculate(corpusDir, separateFilesDir)
    _ <- info[IO]("Context vectors calculated")

    centroidsMaps <- vectorsFiles.toList.traverse { file =>
      for {
        _ <- info[IO](s"Extracting vectors map from ${file.getName}")
        wordToVectorsMap <- readJsonFile[IO, WordToVectorsMap](file)
        _ <- info[IO](s"Creating clusters for ${file.getName}")
        clusters <-  clustersCreator.createClusters(wordToVectorsMap)
      } yield clusters
    }

    _ <- info[IO]("Merging centroids maps")
    wordToCentroidsMap = centroidsMaps.reduce(CollectionUtil.mergeMapsWithUniqueKeys[WordWithPos, Seq[MathVector]])
    _ <- info[IO](s"Created clusters for ${wordToCentroidsMap.size} words")

    meaningsToClustersMap <- mapper.mapClusters(wordToCentroidsMap)
    _ <- info[IO](s"Mapping from meanings to clusters created. Flushing to ${outFile.getAbsolutePath}")
    _ <- writeToFileJson[IO, Map[DictionaryMeaningId, MathVector]](outFile, meaningsToClustersMap)

  } yield ExitCode.Success

}
