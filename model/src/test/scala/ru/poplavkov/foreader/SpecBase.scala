package ru.poplavkov.foreader

import java.nio.file.{Path, Paths}

import com.softwaremill.tagging._
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.specs2.mock.Mockito
import ru.poplavkov.foreader.Generators._
import ru.poplavkov.foreader.Globals._
import ru.poplavkov.foreader.text.Token

/**
  * @author mpoplavkov
  */
trait SpecBase extends AnyWordSpec with Matchers with Mockito

object SpecBase {

  type CovariantId[+A] = A

  def getResourcePath(path: String): Path =
    Paths.get(getClass.getResource(path).toURI)

  def genWords(min: Int, max: Int): Seq[Token.Word] = {
    val count = generate(Gen.chooseNum(min, max))
    (1 to count).map(_ => generate[Token.Word])
  }

  implicit class TaggingHelper(val sc: StringContext) extends AnyVal {
    def w(args: Any*): WordStr = {
      require(args.isEmpty)
      sc.raw().taggedWith[WordStrTag]
    }
  }

}
