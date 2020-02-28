package ru.poplavkov.foreader.dictionary

import ru.poplavkov.foreader.Globals.WordStr

import scala.language.higherKinds

/**
  * Set of multi-word expressions (MWE)
  *
  * @author mpoplavkov
  */
trait MweSet[F[_]] {

  /**
    * Returns all multi-word expressions starting with the `startWord`
    * Each MWE represented as a seq of words in order and doesn't include the `startWord`
    *
    * @param startWord word to lookup MWEs starting with it
    * @return set of MWEs or an empty set if no MWEs found
    */
  def getMwesStartingWith(startWord: WordStr): F[Set[Seq[WordStr]]]

}
