package ru.poplavkov.foreader.text.logged

import ru.poplavkov.foreader.log.LoggingF
import ru.poplavkov.foreader.text.{Token, TokenExtractor}

import scala.language.higherKinds

/**
  * @author mpoplavkov
  */
trait LoggedTokenExtractor[F[_]] extends TokenExtractor[F] with LoggingF[F] {

  abstract override def extract(text: String): F[Seq[Token]] =
    super.extract(text).loggedF("extract", Map("text" -> "possibly huge text"))()

}
