package ru.poplavkov.foreader

import com.softwaremill.tagging._
import org.scalacheck.{Arbitrary, Gen, ScalacheckShapeless}
import ru.poplavkov.foreader.Globals.WordStr

import scala.language.implicitConversions

/**
  * @author mpoplavkov
  */
object Generators extends ScalacheckShapeless {

  implicit def genToArbitrary[A](g: Gen[A]): Arbitrary[A] = Arbitrary(g)

  def generate[A: Arbitrary]: A =
    Iterator.continually(Arbitrary.arbitrary[A].sample).flatten.next()

  def generateSuchThat[A: Arbitrary](condition: A => Boolean): A =
    generate(Arbitrary.arbitrary[A].suchThat(condition))

  implicit lazy val string: Arbitrary[String] = Gen.alphaNumStr
  implicit lazy val wordStr: Arbitrary[WordStr] = Arbitrary.arbitrary[String].map(_.taggedWith)

}
