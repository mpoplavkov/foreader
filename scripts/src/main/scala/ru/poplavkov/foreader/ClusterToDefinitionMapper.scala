package ru.poplavkov.foreader

import java.io.File

import cats.effect.{ExitCode, IO, IOApp, Sync}
import cats.implicits._
import com.softwaremill.tagging._
import io.circe.generic.auto._
import ru.poplavkov.foreader.ClusterToDefinitionMapper.MeaningClusterDist
import ru.poplavkov.foreader.dictionary.Dictionary
import ru.poplavkov.foreader.dictionary.DictionaryEntry.Meaning
import ru.poplavkov.foreader.dictionary.impl.WordNetDictionaryImpl
import ru.poplavkov.foreader.text.impl.CoreNlpTokenExtractor
import ru.poplavkov.foreader.text.{PartOfSpeech, Token, TokenExtractor}
import ru.poplavkov.foreader.vector.{MathVector, VectorsMap}
import ru.poplavkov.foreader.word2vec.VectorsExtractor

import scala.annotation.tailrec
import scala.language.higherKinds

/**
  * @author mpoplavkov
  */
class ClusterToDefinitionMapper[F[_] : Sync](language: Language = Language.English) {

  private val dictionary: Dictionary[F] = new WordNetDictionaryImpl[F]
  private val tokenExtractor: TokenExtractor[F] = new CoreNlpTokenExtractor[F](language)
  private var meaningsWithoutClusterCounter = 0

  def mapClusters(clustersFile: File, vectorsFile: File, contextLen: Int = 3): F[Unit] = {
    val outFile = FileUtil.brotherFile(clustersFile, "clusteredDefinitions.txt")
    val wordPosToClusters = readJsonFile[WordToVectorsMap](clustersFile)

    for {
      vectorsMap <- VectorsExtractor.extractVectors[F](vectorsFile.toPath)
      meaningsToClusters <- wordPosToClusters.toList.traverse { case (wordPos, clusters) =>
        meaningIdToClusterMap(wordPos, clusters, wordPosToClusters, vectorsMap, contextLen)
      }
      allMeaningsToClusters = meaningsToClusters.reduce(CollectionUtil.mergeMapsWithUniqueKeys[String, MathVector])
      _ <- info(s"Meanings without mapped cluster count = $meaningsWithoutClusterCounter")
      _ <- info(s"Mapping from meanings to clusters created. Flushing to ${outFile.getAbsolutePath}")
      _ <- writeToFileJson(outFile, allMeaningsToClusters)
    } yield ()
  }

  private def meaningIdToClusterMap(wordPos: WordWithPos,
                                    clusters: Seq[MathVector],
                                    wordPosToClusters: WordToVectorsMap,
                                    vectorsMap: VectorsMap,
                                    contextLen: Int): F[Map[String, MathVector]] =
    for {
      entry <- dictionary.getDefinition(wordPos.word, wordPos.pos).value
      meanings = entry.toSeq.flatMap(_.meanings)
      _ = require(meanings.size == clusters.size)
      synonymDistances = distancesBySynonymContext(wordPos.pos, meanings, clusters, wordPosToClusters)
      exampleDistances <- distancesByExampleContext(wordPos, vectorsMap, meanings, clusters, contextLen)
      sorted = (synonymDistances ++ exampleDistances).sortBy(_._3)
      meaningIdToClusterMap = meaningToClosestCluster(sorted)

      allMeaningIds = meanings.map(_.id).toSet
      meaningsWithoutCluster = allMeaningIds diff meaningIdToClusterMap.keySet
      restClusters = clusters.toSet diff meaningIdToClusterMap.values.toSet
      resultMap = (meaningsWithoutCluster.toList, restClusters.toList) match {
        case (m :: Nil, cl :: Nil) =>
          meaningIdToClusterMap + (m -> cl)
        case (ms, _) =>
          meaningsWithoutClusterCounter += ms.size
          meaningIdToClusterMap
      }
    } yield resultMap

  @tailrec
  private def meaningToClosestCluster(sortedMeaningClusterDist: Seq[MeaningClusterDist],
                                      alreadyFound: Map[String, MathVector] = Map.empty): Map[String, MathVector] = {
    sortedMeaningClusterDist.headOption match {
      case Some((meaningId, cluster, _)) =>
        val meaningToClusterUpdated = alreadyFound + (meaningId -> cluster)
        val restClustersAndMeanings = sortedMeaningClusterDist
          .filterNot { case (id, clust, _) => clust == cluster || id == meaningId }
        meaningToClosestCluster(restClustersAndMeanings, meaningToClusterUpdated)
      case None =>
        alreadyFound
    }
  }

  private def distancesByExampleContext(wordPos: WordWithPos,
                                        vectorsMap: VectorsMap,
                                        meanings: Seq[Meaning],
                                        clusters: Seq[MathVector],
                                        contextLen: Int): F[Seq[MeaningClusterDist]] =
    meanings.toList.traverse { meaning =>
      examplesContextVectors(meaning, wordPos, vectorsMap, contextLen).map { exampleVectors =>
        for {
          exampleVector <- exampleVectors
          cluster <- clusters
        } yield (meaning.id, cluster, VectorUtil.euclideanDistance(cluster, exampleVector))
      }
    }.map(_.flatten)

  private def examplesContextVectors(meaning: Meaning,
                                     wordPos: WordWithPos,
                                     vectorsMap: VectorsMap,
                                     contextLen: Int): F[Seq[MathVector]] = {
    meaning.examples.toList.traverse { example =>
      for {
        tokens <- tokenExtractor.extract(example)
        wordsWithPos = tokens.collect { case Token.Word(_, _, lemma, pos) => WordWithPos(lemma, pos) }
        wordIndOpt = wordsWithPos.zipWithIndex.find(_._1 == wordPos).map(_._2)
      } yield wordIndOpt.map(contextVectorByIndex(wordsWithPos, _, vectorsMap, contextLen))
    }.map(_.flatten)
  }

  private def distancesBySynonymContext(pos: PartOfSpeech,
                                        meanings: Seq[Meaning],
                                        clusters: Seq[MathVector],
                                        wordPosToClusters: WordToVectorsMap): Seq[MeaningClusterDist] =
    for {
      meaning <- meanings
      synonymVector <- synonymsContextVectors(pos, meaning, wordPosToClusters)
      cluster <- clusters
    } yield (meaning.id, cluster, VectorUtil.euclideanDistance(cluster, synonymVector))

  private def synonymsContextVectors(pos: PartOfSpeech,
                                     meaning: Meaning,
                                     wordPosToClusters: WordToVectorsMap): Seq[MathVector] =
    meaning.synonyms
      .map(s => WordWithPos(s.taggedWith, pos))
      .flatMap(wordPosToClusters.get)
      .flatten

}

object ClusterToDefinitionMapper extends IOApp {

  type MeaningClusterDist = (String, MathVector, Float)

  private val mapper = new ClusterToDefinitionMapper[IO]
  private val lastContextVectorsDir: File = new File(LocalDir)
    .listFiles
    .filter(_.getName.startsWith("context_vectors"))
    .maxBy(_.getName)
  private val clustersFile: File = FileUtil.childFile(lastContextVectorsDir, "clusters.txt")
  private val vectorsFile = new File(s"$LocalDir/vectors.txt")

  override def run(args: List[String]): IO[ExitCode] =
    mapper.mapClusters(clustersFile, vectorsFile).map(_ => ExitCode.Success)
}
