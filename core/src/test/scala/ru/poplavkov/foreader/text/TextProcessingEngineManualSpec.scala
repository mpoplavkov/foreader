package ru.poplavkov.foreader.text

import java.time.Instant

import cats.effect.{IO, Sync}
import cats.syntax.flatMap._
import cats.syntax.functor._
import org.scalatest.Ignore
import ru.poplavkov.foreader.FileUtil._
import ru.poplavkov.foreader.SpecBase.getResourcePath
import ru.poplavkov.foreader.dictionary.impl.{MapDictionaryImpl, MapMweSetImpl}
import ru.poplavkov.foreader.dictionary.wordset.WordsetDictionaryMapExtractor
import ru.poplavkov.foreader.text.empty.{EmptyLevelDeterminator, EmptyLexicalItemGroupFilter}
import ru.poplavkov.foreader.text.impl._
import ru.poplavkov.foreader.{Card, Language, SpecBase}

import scala.language.higherKinds

/**
  * Manual integration test
  *
  * @author mpoplavkov
  */
@Ignore
class TextProcessingEngineManualSpec extends SpecBase {

  def getEngine[F[+_] : Sync]: F[TextProcessingEngine[F]] = {
    val extractor = new WordsetDictionaryMapExtractor[F](
      pathToWordsetDictionary = getResourcePath("/books/wordset")
    )
    for {
      dictMap <- extractor.extractDictionaryMap
      mweMap <- extractor.extractMweMap

      mweSet = new MapMweSetImpl[F](mweMap)
      dictionary = new MapDictionaryImpl[F](dictMap)

      tokenExtractor = new CoreNlpTokenExtractor[F](Language.English)
      stopPhrasesFilter = new StopwordsLexicalItemFilter(
        StopwordsExtractor.extractStopwords(getResourcePath("/books/stop_phrases.txt"))
      )
      stopwordsFilter = new StopwordsLexicalItemFilter(
        StopwordsExtractor.extractStopwords(getResourcePath("/books/stopwords.txt"))
      )
      lexicalItemFilter = new CompositeLexicalItemFilter(Seq(stopwordsFilter, stopPhrasesFilter))
      itemExtractor = new LexicalItemExtractorImpl[F](mweSet)
      levelDeterminator = new EmptyLevelDeterminator[F]
    } yield
      new TextProcessingEngineImpl[F](
        tokenExtractor,
        itemExtractor,
        lexicalItemFilter,
        levelDeterminator,
        dictionary
      )

  }

  "TextProcessingEngine" should {
    "extract cards from book" in {

      val engine = getEngine[IO].unsafeRunSync

      val book = new StringTextRepresentation[IO](
        readFile(getResourcePath("/books/book.txt"))
      )

      val start = Instant.now

      val cards = engine.process(book, EmptyLexicalItemGroupFilter).unsafeRunSync
      cards
        .sortBy(-_.groups.flatMap(_.items).size)
        .take(100)
        .foreach { case Card(lemmas, groups) =>
          val count = groups.flatMap(_.items).size
          println(s"${lemmas.mkString(" ")} [$count] - ${groups.head.definition.meanings.head.definition}")

        }

      val end = Instant.now
      println(s"\n\nTime spent = ${end.getEpochSecond - start.getEpochSecond} seconds")

    }
  }

}
