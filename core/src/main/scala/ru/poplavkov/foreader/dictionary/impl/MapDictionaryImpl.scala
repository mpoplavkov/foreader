package ru.poplavkov.foreader.dictionary.impl

import cats.Applicative
import cats.data.OptionT
import ru.poplavkov.foreader.Globals.WordStr
import ru.poplavkov.foreader.{DictionaryEntry, PartOfSpeech}
import ru.poplavkov.foreader.dictionary.{Dictionary, DictionaryMap}

import scala.language.higherKinds

/**
  * Implementation of the [[Dictionary]] based on the [[DictionaryMap]]
  *
  * @author mpoplavkov
  */
class MapDictionaryImpl[F[_] : Applicative](map: DictionaryMap) extends Dictionary[F] {

  override protected def getDefinitionInternal(words: Seq[WordStr],
                                               partOfSpeech: Option[PartOfSpeech]): OptionT[F, DictionaryEntry] =
    (map.get(words), partOfSpeech) match {
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
