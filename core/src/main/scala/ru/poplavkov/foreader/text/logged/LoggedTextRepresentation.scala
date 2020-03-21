package ru.poplavkov.foreader.text.logged

import cats.data.OptionT
import ru.poplavkov.foreader.log.LoggingF
import ru.poplavkov.foreader.text.TextRepresentation

import scala.language.higherKinds

/**
  * @author mpoplavkov
  */
trait LoggedTextRepresentation[F[_]] extends TextRepresentation[F] with LoggingF[F] {

  abstract override def next: OptionT[F, (String, TextRepresentation[F])] =
    super.next.loggedF("next", Map.empty)()

}
