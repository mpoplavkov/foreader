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

  private val wordWithPosRegexp = "(.*)\\((.*)\\)".r

  implicit val keyEncoder: KeyEncoder[WordWithPos] = (value: WordWithPos) => {
    val WordWithPos(word, pos) = value
    s"$word(${pos.value})"
  }

  implicit val keyDecoder: KeyDecoder[WordWithPos] = {
    case wordWithPosRegexp(word, posStr) =>
      PartOfSpeech.fromString(posStr).map { pos =>
        WordWithPos(word.taggedWith, pos)
      }
    case _ =>
      None
  }

}
