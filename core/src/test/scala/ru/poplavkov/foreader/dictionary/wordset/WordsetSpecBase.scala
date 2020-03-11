package ru.poplavkov.foreader.dictionary.wordset

import cats.Applicative
import ru.poplavkov.foreader.SpecBase
import ru.poplavkov.foreader.SpecBase.getResourcePath

import scala.language.higherKinds

/**
  * @author mpoplavkov
  */
abstract class WordsetSpecBase[F[_] : Applicative] extends SpecBase {

  protected val extractor = new WordsetDictionaryMapExtractor[F](
    pathToWordsetDictionary = getResourcePath("/wordset"),
    fileNames = Set("a.json", "l.json")
  )

}
