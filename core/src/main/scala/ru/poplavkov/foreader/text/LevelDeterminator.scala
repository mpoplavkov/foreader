package ru.poplavkov.foreader.text

import cats.data.OptionT

import scala.language.higherKinds

/**
  * @author mpoplavkov
  */
trait LevelDeterminator[F[_]] {

  def determineLevel(lexicalItem: LexicalItem): OptionT[F, LexicalItemLevel]

}
