package ru.poplavkov.foreader

import com.softwaremill.tagging._
import io.circe.{KeyDecoder, KeyEncoder}
import ru.poplavkov.foreader.Globals.WordStr
import ru.poplavkov.foreader.text.PartOfSpeech

/**
  * @author mpoplavkov
  */
case class WordWithPos(word: WordStr, pos: PartOfSpeech)

object WordWithPos {

  private val Delimiter = "#"

  implicit val keyEncoder: KeyEncoder[WordWithPos] = (value: WordWithPos) => {
    val WordWithPos(word, pos) = value
    s"$word$Delimiter${PartOfSpeech.stringify(pos)}"
  }

  implicit val keyDecoder: KeyDecoder[WordWithPos] = (key: String) =>
    key.split(Delimiter).toList match {
      case word :: posStr :: Nil =>
        PartOfSpeech.fromString(posStr).map { pos =>
          WordWithPos(word.taggedWith, pos)
        }
      case _ =>
        None
    }

}
