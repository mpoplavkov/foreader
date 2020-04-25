package ru.poplavkov.foreader.wsd

import ru.poplavkov.foreader.Globals.DictionaryMeaningId
import ru.poplavkov.foreader.collocation.WordCollocation

/**
  * @author mpoplavkov
  */
case class TrainEntry(collocations: Set[WordCollocation],
                      meaningId: Option[DictionaryMeaningId],
                      docId: DocId)
