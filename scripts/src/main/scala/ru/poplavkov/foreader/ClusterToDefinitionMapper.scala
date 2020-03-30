package ru.poplavkov.foreader

import java.io.File

import cats.effect.{ExitCode, IO, IOApp, Sync}
import cats.implicits._
import com.softwaremill.tagging._
import io.circe.generic.auto._
import ru.poplavkov.foreader.dictionary.Dictionary
import ru.poplavkov.foreader.dictionary.DictionaryEntry.Meaning
import ru.poplavkov.foreader.dictionary.impl.WordNetDictionaryImpl
import ru.poplavkov.foreader.text.impl.CoreNlpTokenExtractor
import ru.poplavkov.foreader.text.{PartOfSpeech, Token, TokenExtractor}
import ru.poplavkov.foreader.vector.{MathVector, VectorsMap}

import scala.annotation.tailrec
import scala.language.higherKinds

/**
  * @author mpoplavkov
  */
class ClusterToDefinitionMapper[F[_] : Sync](language: Language = Language.English) {

  private val dictionary: Dictionary[F] = new WordNetDictionaryImpl[F]
  private val tokenExtractor: TokenExtractor[F] = new CoreNlpTokenExtractor[F](language)

  def mapClusters(clustersFile: File): F[Unit] = {
    val outFile = FileUtil.brotherFile(clustersFile, "clusteredDefinitions.txt")
    val wordPosToClusters = readJsonFile[WordToVectorsMap](clustersFile)
    var meaningsWithoutClusterCounter = 0
    var meaningsWithExample = 0

    val meaningToClusterMap: F[Map[String, MathVector]] =
      wordPosToClusters.toList.traverse { case (WordWithPos(word, pos), clusters) =>
        for {
          entry <- dictionary.getDefinition(word, pos).value
          meanings = entry.toSeq.flatMap(_.meanings)
          _ = require(meanings.size == clusters.size)
          meaningIdToClusterMap = correlateMeaningsToClusters(pos, meanings, clusters, wordPosToClusters)
          allMeaningIds = meanings.map(_.id).toSet
          meaningsWithoutCluster = allMeaningIds diff meaningIdToClusterMap.keySet
          restClusters = clusters.toSet diff meaningIdToClusterMap.values.toSet
          resultMap = (meaningsWithoutCluster.toList, restClusters.toList) match {
            case (m :: Nil, cl :: Nil) =>
              meaningIdToClusterMap + (m -> cl)
            case (ms, _) =>
              meaningsWithoutClusterCounter += ms.size
              meaningsWithExample += meanings.size - ms.size
              meaningIdToClusterMap
            case _ =>
              meaningIdToClusterMap
          }
        } yield resultMap
      }.map(_.reduce(CollectionUtil.mergeMapsWithUniqueKeys[String, MathVector]))

    for {
      map <- meaningToClusterMap
      _ <- info(s"Meanings without mapped cluster count = $meaningsWithoutClusterCounter")
      _ <- info(s"Meanings with example count = $meaningsWithExample")
      _ <- info(s"Mapping from meanings to clusters created. Flushing to ${outFile.getAbsolutePath}")
      _ <- writeToFileJson(outFile, map)
    } yield ()
  }

  private def correlateMeaningsToClusters(pos: PartOfSpeech,
                                          meanings: Seq[Meaning],
                                          clusters: Seq[MathVector],
                                          wordPosToClusters: WordToVectorsMap): Map[String, MathVector] = {
    val clusterMeaningDistSeq: Seq[(MathVector, String, Float)] =
      for {
        meaning <- meanings
        cluster <- clusters
        synVector <- synonymsVectors(pos, meaning, wordPosToClusters)
        dist = VectorUtil.euclideanDistance(cluster, synVector)
      } yield (cluster, meaning.id, dist)

    val nearestClustersAndMeanings = clusterMeaningDistSeq
      .sortBy(_._3)
      .map(tuple => (tuple._1, tuple._2))
    meaningToClosestCluster(nearestClustersAndMeanings)
  }

  @tailrec
  private def meaningToClosestCluster(nearestClustersAndMeanings: Seq[(MathVector, String)],
                                      alreadyFound: Map[String, MathVector] = Map.empty): Map[String, MathVector] = {
    nearestClustersAndMeanings.headOption match {
      case Some((cluster, meaningId)) =>
        val meaningToClusterUpdated = alreadyFound + (meaningId -> cluster)
        val restClustersAndMeanings = nearestClustersAndMeanings
          .filterNot { case (clust, id) => clust == cluster || id == meaningId }
        meaningToClosestCluster(restClustersAndMeanings, meaningToClusterUpdated)
      case None =>
        alreadyFound
    }
  }

  // TODO: write same function for synonyms
  // TODO: refactor scripts

  private def distancesByExamples(wordPos: WordWithPos,
                                  vectorsMap: VectorsMap,
                                  meaning: Meaning,
                                  clusters: Seq[MathVector],
                                  contextLen: Int): F[Seq[(MathVector, MathVector)]] = {
    val examples = meaning.examples
    if (examples.nonEmpty) {
      val contextVectors = examples.toList.traverse { example =>
        for {
          tokens <- tokenExtractor.extract(example)
          wordsWithPos = tokens.collect { case Token.Word(_, _, lemma, pos) => WordWithPos(lemma, pos) }
          wordIndOpt = wordsWithPos.zipWithIndex.find(_._1 == wordPos).map(_._2)
        } yield wordIndOpt.map(contextVectorByIndex(wordsWithPos, _, vectorsMap, contextLen))
      }.map(_.flatten)

      contextVectors.map { vectors =>
        CollectionUtil.cartesianProduct(clusters, vectors)

      }
    } else {
      Seq.empty[(MathVector, MathVector)].pure[F]
    }
  }

  private def synonymsVectors(pos: PartOfSpeech,
                              meaning: Meaning,
                              wordPosToClusters: WordToVectorsMap): Seq[MathVector] =
    meaning.synonyms
      .map(s => WordWithPos(s.taggedWith, pos))
      .flatMap(wordPosToClusters.get)
      .flatten

}

object ClusterToDefinitionMapper extends IOApp {

  type MeaningClusterDist = (Meaning, MathVector, Float)

  val mapper = new ClusterToDefinitionMapper[IO]
  val lastContextVectorsDir: File = new File(LocalDir)
    .listFiles
    .filter(_.getName.startsWith("context_vectors"))
    .maxBy(_.getName)
  val clustersFile: File = FileUtil.childFile(lastContextVectorsDir, "clusters.txt")

  override def run(args: List[String]): IO[ExitCode] =
    mapper.mapClusters(clustersFile).map(_ => ExitCode.Success)
}
