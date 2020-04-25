package ru.poplavkov.foreader.collocation

import ru.poplavkov.foreader.CollectionUtil
import ru.poplavkov.foreader.Globals.DictionaryMeaningId

/**
  * @author mpoplavkov
  */
case class OneItemClassifier(collocationToMeaningSeq: Seq[(WordCollocation, DictionaryMeaningId)])
  extends Function[Set[WordCollocation], Option[DictionaryMeaningId]] {

  override def apply(collocations: Set[WordCollocation]): Option[DictionaryMeaningId] =
    collocationToMeaningSeq
      .find { case (colloc, _) => collocations(colloc) }
      .map(_._2)

}

object OneItemClassifier {

  def fromFacts(facts: Seq[(Set[WordCollocation], DictionaryMeaningId)]): OneItemClassifier = {
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

    val collocationToMeaning = collocMeaningProbabilitySeq
      .sortBy(-_._3)
      .map { case (collocation, meaningId, _) => (collocation, meaningId) }

    new OneItemClassifier(collocationToMeaning)
  }

}
