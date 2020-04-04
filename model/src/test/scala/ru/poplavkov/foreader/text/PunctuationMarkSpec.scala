package ru.poplavkov.foreader.text

import ru.poplavkov.foreader.Generators._
import ru.poplavkov.foreader.SpecBase

/**
  * @author mpoplavkov
  */
class PunctuationMarkSpec extends SpecBase {

  private val mark: PunctuationMark = generate[PunctuationMark]

  "PunctuationMark" should {
    s"convert $mark toString and fromString" in {
      PunctuationMark.fromString(mark.value) shouldBe Some(mark)
    }
  }

}
