package ru.poplavkov.foreader.text.logged

import ru.poplavkov.foreader.Card
import ru.poplavkov.foreader.log.LoggingF
import ru.poplavkov.foreader.text.filter.LexicalItemGroupFilter
import ru.poplavkov.foreader.text.{TextProcessingEngine, TextRepresentation}

import scala.language.higherKinds

/**
  * @author mpoplavkov
  */
trait LoggedTextProcessingEngine[F[_]] extends TextProcessingEngine[F] with LoggingF[F] {

  abstract override def process(text: TextRepresentation[F], filter: LexicalItemGroupFilter): F[Seq[Card]] =
    super.process(text, filter)
      .loggedF("process", Map("text" -> text, "filter" -> filter))()

}
