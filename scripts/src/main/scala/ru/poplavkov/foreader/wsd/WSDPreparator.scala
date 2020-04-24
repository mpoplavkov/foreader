package ru.poplavkov.foreader.wsd

import java.io.File

import cats.effect.Sync
import cats.instances.list._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import io.circe.generic.auto._
import ru.poplavkov.foreader.dictionary.Dictionary
import ru.poplavkov.foreader.text.{LexicalItemExtractor, TokenExtractor}
import ru.poplavkov.foreader.{FileUtil, _}

import scala.language.higherKinds

/**
  * Prepares model for word sense disambiguation
  *
  * @author mpoplavkov
  */
class WSDPreparator[F[_] : Sync](tokenExtractor: TokenExtractor[F],
                                 lexicalItemExtractor: LexicalItemExtractor[F],
                                 dictionary: Dictionary[F]) {

  def prepareWords(corpusDir: File, outDir: File): F[Unit] = {
    val itemsWithDocId = corpusDir.listFiles().toList.traverse { file =>
      val text = FileUtil.readFile(file.toPath)
      val fileId = file.getName
      for {
        sentences <- tokenExtractor.extractSentences(text)
        items <- lexicalItemExtractor.lexicalItemsFromSentences(sentences)
      } yield items.map(i => (i, fileId))
    }.map(_.flatten)

    itemsWithDocId.map { items =>
      items.groupBy(_._1.qualifier).mapValues(_.map { case (item, docId) =>
        (item.context.get, docId)
      })
    }.flatMap(_.toList.traverse { case (qualifier, list) =>
      val outFile = FileUtil.childFile(outDir, s"$qualifier.json")
      Util.writeToFileJson(outFile, list, readable = false)
    }).map(_ => ())

  }

}
