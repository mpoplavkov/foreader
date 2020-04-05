package ru.poplavkov

import java.io.File

import io.circe.{KeyDecoder, KeyEncoder}
import ru.poplavkov.foreader.Globals.DictionaryMeaningId
import ru.poplavkov.foreader.vector.MathVector
import com.softwaremill.tagging._
import ru.poplavkov.foreader.text.impl.ContextExtractorImpl

import scala.language.{higherKinds, implicitConversions}

package object foreader {

  val LocalDir = new File(".local")
  LocalDir.mkdir()
  val TestDataDir: File = FileUtil.childFile(LocalDir, "testdata")
  TestDataDir.mkdir()

  type WordToVectorsMap = Map[WordWithPos, Seq[MathVector]]

  val commonContextExtractor = new ContextExtractorImpl(3)

  implicit val meaningIdKeyEncoder: KeyEncoder[DictionaryMeaningId] = identity

  implicit val meaningIdKeyDecoder: KeyDecoder[DictionaryMeaningId] = s => Some(s.taggedWith)
}
