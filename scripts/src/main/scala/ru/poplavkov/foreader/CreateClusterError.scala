package ru.poplavkov.foreader

/**
  * @author mpoplavkov
  */
sealed trait CreateClusterError

object CreateClusterError {

  case class NoMeaningsInDictionary(wordWithPos: WordWithPos) extends CreateClusterError

  case class TooFewUsageExamples(wordWithPos: WordWithPos,
                                 examples: Int,
                                 definitions: Int) extends CreateClusterError

}
