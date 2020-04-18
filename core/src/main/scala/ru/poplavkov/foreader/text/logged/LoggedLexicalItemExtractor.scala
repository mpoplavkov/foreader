package ru.poplavkov.foreader.text.logged

import ru.poplavkov.foreader.log.LoggingF
import ru.poplavkov.foreader.text.{LexicalItem, LexicalItemExtractor, Token}

import scala.language.higherKinds

/**
  * @author mpoplavkov
  */
trait LoggedLexicalItemExtractor[F[_]] extends LexicalItemExtractor[F] with LoggingF[F] {

  abstract override def lexicalItemsFromSentences(tokensBySentences: Seq[Seq[Token]]): F[Seq[LexicalItem]] =
    super.lexicalItemsFromSentences(tokensBySentences)
      .loggedF("lexicalItemsFromSentences", Map("tokensBySentences" -> tokensBySentences))()

}
