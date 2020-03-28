package ru.poplavkov.foreader

import java.io.{File, FileOutputStream}
import java.nio.charset.Charset
import java.nio.file.{Files, Path}

import cats.effect.{Resource, Sync}

import scala.language.higherKinds

/**
  * @author mpoplavkov
  */
object FileUtil {

  def readFile(path: Path, encoding: Charset = Charset.defaultCharset()): String = {
    val encoded = Files.readAllBytes(path)
    new String(encoded, encoding)
  }

  def writeToFile[F[_] : Sync](file: File, toWrite: String): F[Unit] =
    Resource.fromAutoCloseable(Sync[F].delay(new FileOutputStream(file))).use { os =>
      Sync[F].delay(os.write(toWrite.getBytes))
    }

  def childFile(dir: File, childName: String): File =
    dir.toPath.resolve(childName).toFile

  /**
    * Returns file with the name `brotherName` at the same directory
    */
  def brotherFile(file: File, brotherName: String): File =
    childFile(file.getParentFile, brotherName)

}
