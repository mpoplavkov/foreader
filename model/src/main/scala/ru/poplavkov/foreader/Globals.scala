package ru.poplavkov.foreader

import com.softwaremill.tagging.@@

/**
  * Some global definitions
  *
  * @author mpoplavkov
  */
object Globals {

  trait WordStrTag

  type WordStr = String @@ WordStrTag

  trait DictionaryMeaningIdTag

  type DictionaryMeaningId = String @@ DictionaryMeaningIdTag

}
