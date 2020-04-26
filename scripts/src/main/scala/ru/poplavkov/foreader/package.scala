package ru.poplavkov

import java.io.File

import com.softwaremill.tagging._
import io.circe.{KeyDecoder, KeyEncoder}
import ru.poplavkov.foreader.Globals.{DictionaryMeaningId, Qualifier}

import scala.language.{higherKinds, implicitConversions}

package object foreader {

  val LocalDir = new File(".local")
  LocalDir.mkdir()
  val TestDataDir: File = FileUtil.childFile(LocalDir, "testdata")
  TestDataDir.mkdir()

  implicit val meaningIdKeyEncoder: KeyEncoder[DictionaryMeaningId] = identity
  implicit val meaningIdKeyDecoder: KeyDecoder[DictionaryMeaningId] = s => Some(s.taggedWith)
  implicit val qualifierKeyEncoder: KeyEncoder[Qualifier] = identity
  implicit val qualifierKeyDecoder: KeyDecoder[Qualifier] = s => Some(s.taggedWith)

}
