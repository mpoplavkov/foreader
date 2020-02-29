package ru.poplavkov.foreader.dictionary

import cats.data.OptionT
import ru.poplavkov.foreader.Globals.WordStr

import scala.language.higherKinds

/**
  * @author mpoplavkov
  */
trait Dictionary[F[_]] {

  /**
    * Searches for a definition of a single word
    * @return found [[DictionaryEntry]] or None
    */
  final def getDefinition(word: WordStr): OptionT[F, DictionaryEntry] = getDefinitionInternal(Seq(word))

  /**
    * Searches for a definition of a multi-word expression (MWE)
    * @return found [[DictionaryEntry]] or None
    */
  final def getDefinition(phrase: Seq[WordStr]): OptionT[F, DictionaryEntry] = getDefinitionInternal(phrase)

  protected def getDefinitionInternal(words: Seq[WordStr]): OptionT[F, DictionaryEntry]

}
