package ru.poplavkov.foreader.dictionary

import cats.data.OptionT
import ru.poplavkov.foreader.Globals.WordStr

import scala.language.higherKinds

/**
  * @author mpoplavkov
  */
trait Dictionary[F[_]] {

  final def getDefinition(word: WordStr): OptionT[F, DictionaryEntry] = getDefinitionInternal(Seq(word))

  final def getDefinition(phrase: Seq[WordStr]): OptionT[F, DictionaryEntry] = getDefinitionInternal(phrase)

  protected def getDefinitionInternal(words: Seq[WordStr]): OptionT[F, DictionaryEntry]

}
