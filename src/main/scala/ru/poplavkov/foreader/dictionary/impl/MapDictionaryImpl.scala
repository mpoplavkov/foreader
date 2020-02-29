package ru.poplavkov.foreader.dictionary.impl

import cats.Applicative
import cats.data.OptionT
import ru.poplavkov.foreader.Globals.WordStr
import ru.poplavkov.foreader.dictionary.{Dictionary, DictionaryEntry, DictionaryMap}

import scala.language.higherKinds

/**
  * @author mpoplavkov
  */
class MapDictionaryImpl[F[_] : Applicative](map: DictionaryMap) extends Dictionary[F] {

  override protected def getDefinitionInternal(words: Seq[WordStr]): OptionT[F, DictionaryEntry] =
    OptionT.fromOption(map.get(words))

}
