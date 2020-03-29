package ru.poplavkov.foreader

import java.io.File

import cats.effect.{ExitCode, IO, IOApp, Sync}
import cats.implicits._
import com.softwaremill.tagging._
import io.circe.generic.auto._
import ru.poplavkov.foreader.dictionary.DictionaryEntry.Meaning
import ru.poplavkov.foreader.dictionary.impl.WordNetDictionaryImpl
import ru.poplavkov.foreader.text.PartOfSpeech
import ru.poplavkov.foreader.vector.MathVector

import scala.annotation.tailrec
import scala.language.higherKinds

/**
  * @author mpoplavkov
  */
class ClusterToDefinitionMapper[F[_] : Sync] {

  private val dictionary = new WordNetDictionaryImpl[F]

  def mapClusters(clustersFile: File): F[Unit] = {
    val outFile = FileUtil.brotherFile(clustersFile, "clusteredDefinitions.txt")
    val wordPosToClusters = readJsonFile[WordToVectorsMap](clustersFile)
    var meaningsWithoutClusterCounter = 0

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
              meaningIdToClusterMap
            case _ =>
              meaningIdToClusterMap
          }
        } yield resultMap
      }.map(_.reduce(CollectionUtil.mergeMapsWithUniqueKeys[String, MathVector]))

    for {
      map <- meaningToClusterMap
      _ <- info(s"Meanings without mapped cluster count = $meaningsWithoutClusterCounter")
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

  private def synonymsVectors(pos: PartOfSpeech,
                              meaning: Meaning,
                              wordPosToClusters: WordToVectorsMap): Seq[MathVector] = {
    meaning.synonyms
      .map(s => WordWithPos(s.taggedWith, pos))
      .flatMap(wordPosToClusters.get)
      .flatten
  }

}

object ClusterToDefinitionMapper extends IOApp {

  val mapper = new ClusterToDefinitionMapper[IO]
  val lastContextVectorsDir: File = new File(LocalDir)
    .listFiles
    .filter(_.getName.startsWith("context_vectors"))
    .maxBy(_.getName)
  val clustersFile: File = FileUtil.childFile(lastContextVectorsDir, "clusters.txt")

  override def run(args: List[String]): IO[ExitCode] =
    mapper.mapClusters(clustersFile).map(_ => ExitCode.Success)
}