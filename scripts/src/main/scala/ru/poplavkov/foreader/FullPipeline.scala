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
object FullPipeline extends IOApp {

  private val corpusName = "corpus"
  private val id = Instant.now.toEpochMilli
  private val workDir: File = new File(s"$LocalDir/context_vectors_$id")
  private val separateFilesDir: File = FileUtil.childFile(workDir, "separate")
  workDir.mkdir()
  separateFilesDir.mkdir()
  private val vectorsFile = new File(s"$LocalDir/vectors.txt")
  private val corpusDir = new File(s"$LocalDir/$corpusName")
  private val outFile = FileUtil.childFile(workDir, "clusteredDefinitions.txt")

  private val contextLen = 3
  private val tokenExtractor = new CoreNlpTokenExtractor[IO](Language.English)
  private val dictionary = new WordNetDictionaryImpl[IO](VectorsMap.Empty, Map.empty)
  private val clustersCreator = new ClustersCreator[IO](dictionary)

  override def run(args: List[String]): IO[ExitCode] = for {
    vectorsMap <- VectorsExtractor.extractVectors[IO](vectorsFile.toPath)
    calculator = new ContextVectorsCalculator[IO](tokenExtractor, vectorsMap, dictionary, contextLen)
    mapper = new ClusterToDefinitionMapper[IO](dictionary, tokenExtractor, vectorsMap, contextLen)
    _ <- info[IO]("Vectors extracted")
    vectorsFiles <- calculator.calculate(corpusDir, separateFilesDir)
    _ <- info[IO]("Context vectors calculated")

    centroidsMaps <- vectorsFiles.toList.traverse { file =>
      val wordToVectorsMap = readJsonFile[WordToVectorsMap](file)
      info[IO](s"Creating clusters for ${file.getName}").flatMap { _ =>
        clustersCreator.createClusters(wordToVectorsMap)
      }
    }
    wordToCentroidsMap = centroidsMaps.reduce(CollectionUtil.mergeMapsWithUniqueKeys[WordWithPos, Seq[MathVector]])
    _ <- info[IO](s"Created clusters for ${wordToCentroidsMap.size} words")

    dictionaryToClusterMap <- mapper.mapClusters(wordToCentroidsMap)
    _ <- info[IO](s"Mapping from meanings to clusters created")
    _ <- writeToFileJson[IO, Map[DictionaryMeaningId, MathVector]](outFile, dictionaryToClusterMap)

  } yield ExitCode.Success

}
