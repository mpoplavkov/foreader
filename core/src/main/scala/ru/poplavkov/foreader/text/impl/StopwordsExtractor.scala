package ru.poplavkov.foreader.text.impl

import java.nio.file.Path

import com.softwaremill.tagging._
import ru.poplavkov.foreader.FileUtil._
import ru.poplavkov.foreader.Globals.{WordStr, WordStrTag}

/**
  * @author mpoplavkov
  */
object StopwordsExtractor {

  /**
    * Extracts stopwords from file
    * The contents of the file must contain words or phrases one per line
    */
  def extractStopwords(path: Path): Set[Seq[WordStr]] = {
    readFile(path)
      .split("\n")
      .map(_.split(" ").map(_.taggedWith[WordStrTag]).toSeq)
      .toSet
  }

}
