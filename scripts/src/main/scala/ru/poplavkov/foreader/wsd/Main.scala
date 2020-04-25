package ru.poplavkov.foreader.wsd

import java.io.File

import cats.effect.{ExitCode, IO, IOApp}
import ru.poplavkov.foreader.dictionary.empty.EmptyMweSetImpl
import ru.poplavkov.foreader.dictionary.impl.WordNetDictionaryImpl
import ru.poplavkov.foreader.text.impl.{CoreNlpTokenExtractor, LexicalItemExtractorImpl}
import ru.poplavkov.foreader.vector.VectorsMap
import ru.poplavkov.foreader.{Language, _}

/**
  * @author mpoplavkov
  */
object Main extends IOApp {

  val corpusDir: File = FileUtil.childFile(LocalDir, "corpus")
  val outDir: File = FileUtil.childFile(LocalDir, "words")
  outDir.mkdir()

  val tokenExtractor = new CoreNlpTokenExtractor[IO](Language.English)
  val mweSet = new EmptyMweSetImpl[IO]
  val lexicalItemExtractor = new LexicalItemExtractorImpl[IO](mweSet)
  val dictionary = new WordNetDictionaryImpl[IO](VectorsMap.Empty, Map.empty)
  val preparator = new WSDPreparator[IO](tokenExtractor, lexicalItemExtractor, dictionary, k = 3)

  override def run(args: List[String]): IO[ExitCode] =
    preparator.prepareWords(corpusDir, outDir)
      .map(_ => ExitCode.Success)
}
