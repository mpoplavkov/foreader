package ru.poplavkov.foreader

import com.softwaremill.tagging._
import io.circe.{Decoder, Encoder}

/**
  * Some global definitions
  *
  * @author mpoplavkov
  */
object Globals {

  trait WordStrTag

  type WordStr = String @@ WordStrTag

  trait DictionaryMeaningIdTag

  type DictionaryMeaningId = String @@ DictionaryMeaningIdTag

  implicit def taggedStringEncoder[T]: Encoder[String @@ T] = Encoder.encodeString.contramap(identity)

  implicit def taggedStringDecoder[T]: Decoder[String @@ T] = Decoder.decodeString.map(_.taggedWith[T])

}
