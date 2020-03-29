package ru.poplavkov.foreader.text

import io.circe.syntax._
import ru.poplavkov.foreader.Generators._
import ru.poplavkov.foreader.SpecBase

/**
  * @author mpoplavkov
  */
class PartOfSpeechSpec extends SpecBase {

  private val pos: PartOfSpeech = generate[PartOfSpeech]

  "PartOfSpeech" should {
    s"convert $pos" in {
      pos.asJson.as[PartOfSpeech].toOption.get shouldBe pos
    }
  }

}
