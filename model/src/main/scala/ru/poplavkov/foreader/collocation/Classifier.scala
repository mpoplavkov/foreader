package ru.poplavkov.foreader.collocation

import ru.poplavkov.foreader.Globals.DictionaryMeaningId

/**
  * @author mpoplavkov
  */
case class Classifier(collocationToMeaningSeq: Seq[(WordCollocation, DictionaryMeaningId)])
  extends Function[Set[WordCollocation], Option[DictionaryMeaningId]] {

  override def apply(collocations: Set[WordCollocation]): Option[DictionaryMeaningId] =
    collocationToMeaningSeq
      .find { case (colloc, _) => collocations(colloc) }
      .map(_._2)

}

object Classifier {

  def apply(facts: Seq[(Set[WordCollocation], DictionaryMeaningId)]): Classifier = {
    val collocMeaningProbabilitySeq = facts.flatMap { case (collocations, meaningId) =>
      collocations.map(c => (c, meaningId))
    }.groupBy(_._1)
      .mapValues(_.map(_._2))
      .mapValues { meaningIds =>
        val mostProbableMeaningWithSize = meaningIds.groupBy(identity).mapValues(_.size).maxBy(_._2)
        val bestMeaning = mostProbableMeaningWithSize._1
        val probability = mostProbableMeaningWithSize._2.toDouble / meaningIds.size
        bestMeaning -> probability
      }.map { case (collocation, (meaningId, probability)) => (collocation, meaningId, probability) }
      .toSeq

    val collocationToMeaning = collocMeaningProbabilitySeq
      .sortBy(-_._3)
      .map { case (collocation, meaningId, _) => (collocation, meaningId) }

    new Classifier(collocationToMeaning)
  }

}
