package ru.poplavkov.foreader.dictionary.impl

import cats.Applicative
import cats.data.OptionT
import ru.poplavkov.foreader.dictionary.{Dictionary, DictionaryEntry, DictionaryMap}
import ru.poplavkov.foreader.text.LexicalItem

import scala.language.higherKinds

/**
  * Implementation of the [[Dictionary]] based on the [[DictionaryMap]]
  *
  * @author mpoplavkov
  */
class DictionaryImpl[F[_] : Applicative](map: DictionaryMap) extends Dictionary[F] {

  override def getDefinition(lexicalItem: LexicalItem): OptionT[F, DictionaryEntry] =
    (map.get(lexicalItem.lemmas), lexicalItem.partsOfSpeech.headOption) match {
      case (None, _) =>
        OptionT.none
      case (Some(entry), None) =>
        OptionT.some(entry)
      case (Some(entry), Some(pos)) =>
        val suitableEntry = entry.filterMeanings(_.partOfSpeech.contains(pos))
        OptionT.some {
          if (suitableEntry.isEmpty) entry else suitableEntry
        }
    }

}
