package ru.poplavkov.foreader

import java.nio.file.{Path, Paths}

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

/**
  * @author mpoplavkov
  */
trait SpecBase extends AnyWordSpec with Matchers {

  def getResourcePath(path: String): Path =
    Paths.get(getClass.getResource(path).toURI)

}
