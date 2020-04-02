package ru.poplavkov.foreader.stages

import cats.effect.Sync
import cats.instances.list._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import com.softwaremill.tagging._
import ru.poplavkov.foreader.Globals.DictionaryMeaningId
import ru.poplavkov.foreader.Util._
import ru.poplavkov.foreader._
import ru.poplavkov.foreader.dictionary.Dictionary
import ru.poplavkov.foreader.dictionary.DictionaryEntry.Meaning
import ru.poplavkov.foreader.stages.ClusterToDefinitionMapper.MeaningClusterDist
import ru.poplavkov.foreader.text.{PartOfSpeech, Token, TokenExtractor}
import ru.poplavkov.foreader.vector.{MathVector, VectorsMap}

import scala.annotation.tailrec
import scala.language.higherKinds

/**
  * @author mpoplavkov
  */
class ClusterToDefinitionMapper[F[_] : Sync](dictionary: Dictionary[F],
                                             tokenExtractor: TokenExtractor[F],
                                             vectorsMap: VectorsMap,
                                             contextLen: Int) {

  private var meaningsWithoutClusterCounter = 0

  def mapClusters(wordPosToClusters: WordToVectorsMap): F[Map[DictionaryMeaningId, MathVector]] = {
    for {
      meaningsToClusters <- wordPosToClusters.toList.traverse { case (wordPos, clusters) =>
        meaningIdToClusterMap(wordPos, clusters, wordPosToClusters, vectorsMap)
      }
      _ <- info(s"Meanings without mapped cluster count = $meaningsWithoutClusterCounter")
      allMeaningsToClusters = meaningsToClusters.reduce(CollectionUtil.mergeMapsWithUniqueKeys[DictionaryMeaningId, MathVector])
    } yield allMeaningsToClusters
  }

  private def meaningIdToClusterMap(wordPos: WordWithPos,
                                    clusters: Seq[MathVector],
                                    wordPosToClusters: WordToVectorsMap,
                                    vectorsMap: VectorsMap): F[Map[DictionaryMeaningId, MathVector]] =
    for {
      entry <- dictionary.getDefinition(wordPos.word, wordPos.pos).value
      meanings = entry.toSeq.flatMap(_.meanings)
      _ = require(meanings.size == clusters.size)
      synonymDistances = distancesBySynonymContext(wordPos.pos, meanings, clusters, wordPosToClusters)
      exampleDistances <- distancesByExampleContext(wordPos, vectorsMap, meanings, clusters)
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
                                      alreadyFound: Map[DictionaryMeaningId, MathVector] = Map.empty): Map[DictionaryMeaningId, MathVector] = {
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
                                        clusters: Seq[MathVector]): F[Seq[MeaningClusterDist]] =
    meanings.toList.traverse { meaning =>
      examplesContextVectors(meaning, wordPos, vectorsMap).map { exampleVectors =>
        for {
          exampleVector <- exampleVectors
          cluster <- clusters
        } yield (meaning.id, cluster, VectorUtil.euclideanDistance(cluster, exampleVector))
      }
    }.map(_.flatten)

  private def examplesContextVectors(meaning: Meaning,
                                     wordPos: WordWithPos,
                                     vectorsMap: VectorsMap): F[Seq[MathVector]] = {
    meaning.examples.toList.traverse { example =>
      for {
        tokens <- tokenExtractor.extract(example)
        wordIndOpt = tokens.zipWithIndex.collectFirst {
          case (Token.Word(_, _, word, pos), ind) if WordWithPos(word, pos) == wordPos => ind
        }
      } yield wordIndOpt.map(contextVectorByIndex(tokens, _, vectorsMap, contextLen))
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

object ClusterToDefinitionMapper {

  type MeaningClusterDist = (DictionaryMeaningId, MathVector, Float)

}
