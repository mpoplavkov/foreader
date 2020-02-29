package ru.poplavkov.foreader.dictionary

import cats.Functor
import cats.syntax.functor._

import scala.language.higherKinds

/**
  * @author mpoplavkov
  */
abstract class DictionaryMapExtractor[F[_] : Functor] {

  def extractDictionaryMap: F[DictionaryMap]

  final def extractMweMap: F[MweMap] = extractDictionaryMap.map(dictionaryMapToMweMap)

}
