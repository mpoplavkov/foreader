package ru.poplavkov.foreader.wsd

import java.io.File

import cats.effect.{ExitCode, IO, IOApp}
import ru.poplavkov.foreader.dictionary.empty.EmptyMweSetImpl
import ru.poplavkov.foreader.dictionary.impl.WordNetDictionaryImpl
import ru.poplavkov.foreader.text.impl.{CoreNlpTokenExtractor, LexicalItemExtractorImpl}
import ru.poplavkov.foreader.vector.VectorsMap
import ru.poplavkov.foreader.word2vec.VectorsExtractor
import ru.poplavkov.foreader.{Language, _}

/**
  * @author mpoplavkov
  */
object Main extends IOApp {

  val corpusDir: File = FileUtil.childFile(LocalDir, "corpus")
  val outDir: File = FileUtil.childFile(LocalDir, "words")
  outDir.mkdir()
  val vectorsFile: File = FileUtil.childFile(LocalDir, "vectors.txt")

  val tokenExtractor = new CoreNlpTokenExtractor[IO](Language.English)
  val mweSet = new EmptyMweSetImpl[IO]
  val lexicalItemExtractor = new LexicalItemExtractorImpl[IO](mweSet)
  val dictionary = new WordNetDictionaryImpl[IO](VectorsMap.Empty, Map.empty)

  override def run(args: List[String]): IO[ExitCode] =
    for {
      vectorsMap <- VectorsExtractor.extractVectors[IO](vectorsFile.toPath)
      preparator = new WSDPreparator[IO](tokenExtractor, lexicalItemExtractor, dictionary, vectorsMap, k = 3)

      _ <- preparator.prepareWords(corpusDir, outDir)
    } yield ExitCode.Success
}
