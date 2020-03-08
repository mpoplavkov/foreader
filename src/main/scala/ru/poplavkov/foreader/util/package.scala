package ru.poplavkov.foreader

import java.nio.charset.Charset
import java.nio.file.{Files, Path}

package object util {

  def readFile(path: Path, encoding: Charset = Charset.defaultCharset()): String = {
    val encoded = Files.readAllBytes(path)
    new String(encoded, encoding)
  }

}
