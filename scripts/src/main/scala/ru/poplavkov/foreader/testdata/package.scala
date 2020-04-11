package ru.poplavkov.foreader

import ru.poplavkov.foreader.Globals.DictionaryMeaningId

package object testdata {

  type Answers = Map[String, Seq[DictionaryMeaningId]]

  val EmptyAnswers = Map.empty[String, Seq[DictionaryMeaningId]]

}
