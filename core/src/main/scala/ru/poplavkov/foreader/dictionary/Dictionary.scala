package ru.poplavkov.foreader.dictionary

import cats.data.OptionT
import ru.poplavkov.foreader.Globals.WordStr
import ru.poplavkov.foreader.text.{LexicalItem, PartOfSpeech, Token}

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

  final def getDefinition(word: WordStr, partOfSpeech: PartOfSpeech): OptionT[F, DictionaryEntry] = {
    val token = Token.Word(0, word, word, partOfSpeech)
    val item = LexicalItem.SingleWord(token)
    getDefinition(item)
  }

}
