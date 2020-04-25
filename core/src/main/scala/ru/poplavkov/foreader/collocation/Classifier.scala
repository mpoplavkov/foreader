package ru.poplavkov.foreader.collocation

import ru.poplavkov.foreader.Globals.{DictionaryMeaningId, Qualifier}

/**
  * @author mpoplavkov
  */
class Classifier(qualifiersToClassifier: Map[Qualifier, OneItemClassifier]) {

  def apply(qualifier: Qualifier, collocations: Set[WordCollocation]): Option[DictionaryMeaningId] =
    qualifiersToClassifier.get(qualifier).flatMap(_.apply(collocations))

}
