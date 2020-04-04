package ru.poplavkov.foreader.text

import ru.poplavkov.foreader.Generators._
import ru.poplavkov.foreader.SpecBase

/**
  * @author mpoplavkov
  */
class PartOfSpeechSpec extends SpecBase {

  private val pos: PartOfSpeech = generate[PartOfSpeech]

  "PartOfSpeech" should {
    s"convert $pos toString and fromString" in {
      PartOfSpeech.fromString(PartOfSpeech.stringify(pos)) shouldBe Some(pos)
    }
  }
}
