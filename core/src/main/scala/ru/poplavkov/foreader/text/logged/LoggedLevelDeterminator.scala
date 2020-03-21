package ru.poplavkov.foreader.text.logged

import cats.data.OptionT
import ru.poplavkov.foreader.log.LoggingF
import ru.poplavkov.foreader.text.{LevelDeterminator, LexicalItem, LexicalItemLevel}

import scala.language.higherKinds

/**
  * @author mpoplavkov
  */
trait LoggedLevelDeterminator[F[_]] extends LevelDeterminator[F] with LoggingF[F] {

  abstract override def determineLevel(lexicalItem: LexicalItem): OptionT[F, LexicalItemLevel] =
    super.determineLevel(lexicalItem)
      .loggedF("determineLevel", Map("lexicalItem" -> lexicalItem))()

}
