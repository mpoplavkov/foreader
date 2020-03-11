package ru.poplavkov.foreader.text

import cats.data.OptionT
import ru.poplavkov.foreader.{LexicalItem, LexicalItemLevel}

import scala.language.higherKinds

/**
  * @author mpoplavkov
  */
trait LevelDeterminator[F[_]] {

  def determineLevel(lexicalItem: LexicalItem): OptionT[F, LexicalItemLevel]

}
