package ru.poplavkov.foreader.text

import java.time.Instant

import cats.effect.{IO, Sync}
import cats.syntax.flatMap._
import cats.syntax.functor._
import org.scalatest.Ignore
import ru.poplavkov.foreader.FileUtil._
import ru.poplavkov.foreader.SpecBase.getResourcePath
import ru.poplavkov.foreader.dictionary.impl.{DictionaryImpl, MapMweSetImpl}
import ru.poplavkov.foreader.dictionary.wordset.WordsetDictionaryMapExtractor
import ru.poplavkov.foreader.text.empty.EmptyLevelDeterminator
import ru.poplavkov.foreader.text.filter.empty.EmptyLexicalItemGroupFilter
import ru.poplavkov.foreader.text.filter.impl.{CompositeLexicalItemFilter, StopwordsLexicalItemFilter}
import ru.poplavkov.foreader.text.impl._
import ru.poplavkov.foreader.text.logged.LoggedLexicalItemFilter
import ru.poplavkov.foreader.word2vec.VectorsExtractor
import ru.poplavkov.foreader.{Language, SpecBase}

import scala.language.higherKinds

/**
  * Manual integration test
  *
  * @author mpoplavkov
  */
@Ignore
class TextProcessingEngineManualSpec extends SpecBase {

  val text: String = readFile(getResourcePath("/books/.local/alice.txt")).take(500)

  def getEngine[F[+ _] : Sync]: F[TextProcessingEngine[F]] = {
    val extractor = new WordsetDictionaryMapExtractor[F](
      pathToWordsetDictionary = getResourcePath("/books/.local/wordset")
    )
    for {
      dictMap <- extractor.extractDictionaryMap
      mweMap <- extractor.extractMweMap

      mweSet = new MapMweSetImpl[F](mweMap)
      vectors <- VectorsExtractor.extractVectors[F](getResourcePath("/books/.local/word2vec/glove.6B.50d.txt"))
      dictionary = new DictionaryImpl[F](dictMap, vectors)

      tokenExtractor = new CoreNlpTokenExtractor[F](Language.English)
      stopPhrasesFilter = new StopwordsLexicalItemFilter(
        StopwordsExtractor.extractStopwords(getResourcePath("/books/stop_phrases.txt"))
      )
      stopwordsFilter = new StopwordsLexicalItemFilter(
        StopwordsExtractor.extractStopwords(getResourcePath("/books/stopwords.txt"))
      )
      lexicalItemFilter = new CompositeLexicalItemFilter(Seq(stopwordsFilter, stopPhrasesFilter))
        with LoggedLexicalItemFilter
      itemExtractor = new LexicalItemExtractorImpl[F](mweSet)
      levelDeterminator = new EmptyLevelDeterminator[F]
    } yield {
      new TextProcessingEngineImpl[F](
        tokenExtractor,
        itemExtractor,
        lexicalItemFilter,
        levelDeterminator,
        dictionary
      )
    }

  }

  "TextProcessingEngine" should {
    "extract cards from book" in {

      val engine = getEngine[IO].unsafeRunSync

      val book = new StringTextRepresentation[IO](text)

      val start = Instant.now

      val cards = engine.process(book, EmptyLexicalItemGroupFilter).unsafeRunSync

      val pairs = for {
        card <- cards
        group <- card.groups
        item <- group.items
      } yield (item, group.definition)

      pairs
        .sortBy { case (item, _) =>
          item match {
            case LexicalItem.SingleWord(word, _) => word.position
            case LexicalItem.MultiWordExpression(words, _) => words.head.position
          }
        }
        .take(100)
        .foreach { case (item, entry) =>
          val len = 20
          val str = item.lemmas.mkString(" ")
          val diff = len - str.length
          val rest = Math.max(diff, 0)
          val spaces = " " * rest
          println(s"$str$spaces - ${entry.meanings.head.definition}")
        }

      val end = Instant.now
      println(s"\n\nTime spent = ${end.getEpochSecond - start.getEpochSecond} seconds")

    }
  }

}
