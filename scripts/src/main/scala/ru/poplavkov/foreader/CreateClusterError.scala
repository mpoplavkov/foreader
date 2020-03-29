package ru.poplavkov.foreader

/**
  * @author mpoplavkov
  */
sealed trait CreateClusterError

object CreateClusterError {

  case class NoMeaningsInDictionary(word: String) extends CreateClusterError

  case class TooFewUsageExamples(word: String, examples: Int, definitions: Int) extends CreateClusterError

}
