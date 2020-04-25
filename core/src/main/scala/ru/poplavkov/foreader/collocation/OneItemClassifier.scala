package ru.poplavkov.foreader.collocation

import ru.poplavkov.foreader.Globals.DictionaryMeaningId
import ru.poplavkov.foreader.vector.{MathVector, VectorsMap}
import ru.poplavkov.foreader.{CollectionUtil, VectorUtil}

/**
  * @author mpoplavkov
  */
class OneItemClassifier(val collocationToMeaningSeq: Seq[(WordCollocation, DictionaryMeaningId)],
                        vectorsMap: VectorsMap,
                        threshold: Double)
  extends Function[Set[WordCollocation], Option[DictionaryMeaningId]] {

  private val vectorsToMeanings: Seq[(WordCollocation, Seq[MathVector], DictionaryMeaningId)] =
    collocationToMeaningSeq.map { case (collocation, meaningId) =>
      (collocation, collocation2Vectors(collocation), meaningId)
    }

  override def apply(collocations: Set[WordCollocation]): Option[DictionaryMeaningId] =
    vectorsToMeanings.find { case (colloc, vectors, _) =>
      if (vectors.isEmpty) {
        // exact match
        collocations(colloc)
      } else {
        val collocVectors = collocation2Vectors(colloc)
        collocVectors.size == vectors.size &&
          collocVectors.zip(vectors).forall { case (a, b) =>
            VectorUtil.euclideanDistance(a, b) <= threshold
          }
      }
    }
      .map(_._3)

  private def collocation2Vectors(collocation: WordCollocation): Seq[MathVector] =
    collocation.words.flatMap(vectorsMap.getVector)
}

object OneItemClassifier {

  def fromFacts(facts: Seq[(Set[WordCollocation], DictionaryMeaningId)],
                minProbability: Double,
                depth: Option[Int],
                vectorsMap: VectorsMap,
                vectorSimilarityThreshold: Double): OneItemClassifier = {
    val collocMeaningProbabilitySeq = facts.flatMap { case (collocations, meaningId) =>
      collocations.map(c => (c, meaningId))
    }.groupBy(_._1)
      .mapValues(_.map(_._2))
      .mapValues { meaningIds =>
        val mostProbableMeaningWithSize = CollectionUtil.mostFrequentElement(meaningIds)
        val bestMeaning = mostProbableMeaningWithSize._1
        val probability = mostProbableMeaningWithSize._2.toDouble / meaningIds.size
        bestMeaning -> probability
      }.map { case (collocation, (meaningId, probability)) => (collocation, meaningId, probability) }
      .toSeq
      .filter(_._3 > minProbability)

    val collocationToMeaning = collocMeaningProbabilitySeq
      .sortBy(-_._3)
      .map { case (collocation, meaningId, _) => (collocation, meaningId) }

    val withLimit = depth.fold(collocationToMeaning)(limit => collocationToMeaning.take(limit))

    new OneItemClassifier(withLimit, vectorsMap, vectorSimilarityThreshold)
  }

}
