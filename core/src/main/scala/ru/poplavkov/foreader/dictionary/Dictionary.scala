package ru.poplavkov.foreader.dictionary

import cats.data.OptionT
import ru.poplavkov.foreader.text.LexicalItem

import scala.language.higherKinds

/**
  * @author mpoplavkov
  */
trait Dictionary[F[_]] {

  /**
    * Searches for a definition of a lexical item
    *
    * @return found [[DictionaryEntry]] or None
    */
  def getDefinition(lexicalItem: LexicalItem): OptionT[F, DictionaryEntry]

}
