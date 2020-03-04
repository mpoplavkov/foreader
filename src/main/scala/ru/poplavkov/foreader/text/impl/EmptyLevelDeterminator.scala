package ru.poplavkov.foreader.text.impl

import cats.Applicative
import cats.data.OptionT
import ru.poplavkov.foreader.LexicalItem
import ru.poplavkov.foreader.text.{LevelDeterminator, LexicalItemLevel}

import scala.language.higherKinds

/**
  * @author mpoplavkov
  */
class EmptyLevelDeterminator[F[_] : Applicative] extends LevelDeterminator[F] {

  override def determineLevel(lexicalItem: LexicalItem): OptionT[F, LexicalItemLevel] =
    OptionT.none

}
