package ru.poplavkov.foreader.text.logged

import ru.poplavkov.foreader.log.LoggingF
import ru.poplavkov.foreader.text.{Token, TokenExtractor}

import scala.language.higherKinds

/**
  * @author mpoplavkov
  */
trait LoggedTokenExtractor[F[_]] extends TokenExtractor[F] with LoggingF[F] {

  abstract override def extractSentences(text: String): F[Seq[Seq[Token]]] =
    super.extractSentences(text).loggedF("extractSentences", Map("text" -> "possibly huge text"))()

}
