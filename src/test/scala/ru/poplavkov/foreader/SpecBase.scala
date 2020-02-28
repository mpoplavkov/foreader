package ru.poplavkov.foreader

import java.nio.file.{Path, Paths}

import com.softwaremill.tagging._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import ru.poplavkov.foreader.Globals._

/**
  * @author mpoplavkov
  */
trait SpecBase extends AnyWordSpec with Matchers

object SpecBase {

  def getResourcePath(path: String): Path =
    Paths.get(getClass.getResource(path).toURI)

  implicit class TaggingHelper(val sc: StringContext) extends AnyVal {
    def w(args: Any*): WordStr = {
      require(args.isEmpty)
      sc.raw().taggedWith[WordStrTag]
    }
  }
}
