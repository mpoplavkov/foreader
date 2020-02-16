package ru.poplavkov.foreader

import com.softwaremill.tagging._
import org.scalacheck.Gen

/**
  * @author mpoplavkov
  */
object Generators {

  implicit def generate[A](implicit gen: Gen[A]): A =
    Iterator.continually(gen.sample).flatten.next()

  implicit def generateSuchThat[A](condition: A => Boolean)(implicit gen: Gen[A]): A =
    generate(gen.suchThat(condition))

  implicit val StringGen: Gen[String] = Gen.alphaNumStr

  implicit def taggedGen[A: Gen, T]: Gen[A @@ T] =
    generate[A].taggedWith[T]

}
