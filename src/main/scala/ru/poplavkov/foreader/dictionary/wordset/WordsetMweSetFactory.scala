package ru.poplavkov.foreader.dictionary.wordset

import java.nio.charset.Charset
import java.nio.file.{Files, Paths}

import cats.Applicative
import cats.syntax.applicative._
import com.softwaremill.tagging._
import io.circe.parser._
import ru.poplavkov.foreader.Globals.{Word, WordTag}
import ru.poplavkov.foreader.dictionary.wordset.WordsetMweSetFactory._
import ru.poplavkov.foreader.dictionary.{MapMweSetImpl, MweSet, MweSetFactory}

import scala.language.higherKinds

/**
  * MweSetFactory based on the wordset dictionary
  * Wordset dictionary is a collection of json files `a.json` ... `z.json`
  * containing words with definitions
  * Purpose of that class is to extract all dictionary keys consisting of
  * more than one word and group them by the first word
  *
  * @see https://github.com/wordset/wordset-dictionary
  * @author mpoplavkov
  */
class WordsetMweSetFactory[F[_] : Applicative](pathToWordsetDictionary: String) extends MweSetFactory[F] {

  override def createMweSet(): F[MweSet[F]] = {

    val mapsSet: Set[Map[Word, Set[List[Word]]]] = for {
      fileName <- FileNamesSet
      fileContent = readFile(s"$pathToWordsetDictionary/$fileName")
      jsonFileContent <- parse(fileContent).toOption
      jsonObject <- jsonFileContent.asObject
    } yield jsonObject.keys
      .map(_.split(" ").map(_.taggedWith[WordTag]))
      .filter(_.length > 1)
      .groupBy(_.head)
      .mapValues(_.map(_.tail.toList).toSet)

    val mweSet: MweSet[F] = new MapMweSetImpl(mapsSet.reduce(_ ++ _))

    mweSet.pure
  }

}

object WordsetMweSetFactory {

  private val FileNamesSet: Set[String] = ('a' to 'z').map(_ + ".json").toSet

  private def readFile(path: String, encoding: Charset = Charset.defaultCharset()): String = {
    val encoded = Files.readAllBytes(Paths.get(path))
    new String(encoded, encoding)
  }

}
