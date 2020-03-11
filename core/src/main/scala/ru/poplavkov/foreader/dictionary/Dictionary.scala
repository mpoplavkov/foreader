package ru.poplavkov.foreader.dictionary

import cats.data.OptionT
import ru.poplavkov.foreader.Globals.WordStr
import ru.poplavkov.foreader.{DictionaryEntry, PartOfSpeech}

import scala.language.higherKinds

/**
  * @author mpoplavkov
  */
trait Dictionary[F[_]] {

  /**
    * Searches for a definition of a single word
    *
    * @return found [[DictionaryEntry]] or None
    */
  final def getDefinition(word: WordStr,
                          partOfSpeech: Option[PartOfSpeech]): OptionT[F, DictionaryEntry] =
    getDefinitionInternal(Seq(word), partOfSpeech)

  /**
    * Searches for a definition of a multi-word expression (MWE)
    *
    * @return found [[DictionaryEntry]] or None
    */
  final def getDefinition(phrase: Seq[WordStr],
                          partOfSpeech: Option[PartOfSpeech]): OptionT[F, DictionaryEntry] =
    getDefinitionInternal(phrase, partOfSpeech)

  protected def getDefinitionInternal(words: Seq[WordStr],
                                      partOfSpeech: Option[PartOfSpeech]): OptionT[F, DictionaryEntry]

}