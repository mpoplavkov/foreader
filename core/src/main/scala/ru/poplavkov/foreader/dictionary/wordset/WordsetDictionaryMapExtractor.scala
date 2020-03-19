package ru.poplavkov.foreader.dictionary.wordset

import java.nio.file.Path

import cats.Applicative
import cats.syntax.applicative._
import com.softwaremill.tagging._
import io.circe.Json
import io.circe.parser.parse
import ru.poplavkov.foreader.FileUtil._
import ru.poplavkov.foreader.Globals.WordStrTag
import ru.poplavkov.foreader.dictionary.wordset.WordsetDictionaryMapExtractor._
import ru.poplavkov.foreader.dictionary.{DictionaryEntry, DictionaryMap, DictionaryMapExtractor}
import ru.poplavkov.foreader.text.PartOfSpeech

import scala.language.higherKinds

/**
  * [[DictionaryMapExtractor]] based on the wordset dictionary
  * Wordset dictionary is a collection of json files `a.json` ... `z.json`
  * containing words with definitions
  * Purpose of that class is to extract all dictionary entries from the
  * wordset dictionary
  *
  * @see https://github.com/wordset/wordset-dictionary
  * @author mpoplavkov
  */
class WordsetDictionaryMapExtractor[F[_] : Applicative](pathToWordsetDictionary: Path,
                                                        fileNames: Set[String] = FileNamesSet)
  extends DictionaryMapExtractor[F] {

  override def extractDictionaryMap: F[DictionaryMap] = {

    val dictMap: DictionaryMap =
      (for {
        fileName <- fileNames
        fileContent = readFile(pathToWordsetDictionary.resolve(fileName))
        jsonFileContent <- parse(fileContent).toOption.toSeq
        jsonObject <- jsonFileContent.asObject.toSeq
        (key, entry) <- jsonObject.toList
        dictEntry <- extractDictionaryEntryFromJson(entry)
        words = key.split(" ").map(_.taggedWith[WordStrTag])
      } yield words.toSeq -> dictEntry).toMap

    dictMap.pure
  }

}

object WordsetDictionaryMapExtractor {

  private val FileNamesSet: Set[String] = ('a' to 'z').map(_ + ".json").toSet

  private def extractDictionaryEntryFromJson(json: Json): Option[DictionaryEntry] = {
    val meaningsArray = for {
      obj <- json.asObject
      meaningsJson <- obj("meanings")
      arr <- meaningsJson.asArray
    } yield arr

    val meanings = meaningsArray.toSeq.flatten.flatMap(extractMeaningFromJson)
    if (meanings.isEmpty) {
      None
    } else {
      Some(DictionaryEntry(meanings = meanings))
    }
  }

  private def extractMeaningFromJson(json: Json): Option[DictionaryEntry.Meaning] = {

    for {
      obj <- json.asObject
      definition <- obj("def")
      defString <- definition.asString
    } yield {
      val partOfSpeechOpt = obj("speech_part")
        .flatMap(_.asString)
        .flatMap(toPartOfSpeech)

      val examples = obj("example")
        .flatMap(_.asString)
        .toSeq

      val synonyms = obj("synonyms")
        .flatMap(_.asArray)
        .toSeq
        .flatten
        .flatMap(_.asString)

      DictionaryEntry.Meaning(
        definition = defString,
        partOfSpeech = partOfSpeechOpt,
        examples = examples,
        synonyms = synonyms
      )
    }

  }

  // TODO: complete
  private def toPartOfSpeech(mwePos: String): Option[PartOfSpeech] = mwePos match {
    case "adjective" => Some(PartOfSpeech.Adjective)
    case "adverb" => Some(PartOfSpeech.Adverb)
    case "noun" => Some(PartOfSpeech.Noun)
    case "verb" => Some(PartOfSpeech.Verb)
    case _ => None
  }

}
