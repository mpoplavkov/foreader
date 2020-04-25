package ru.poplavkov.foreader.wsd

import java.io.File

import cats.effect.Sync
import cats.instances.list._
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import io.circe.generic.auto._
import ru.poplavkov.foreader.dictionary.Dictionary
import ru.poplavkov.foreader.text.{LexicalItemExtractor, TextContext, TokenExtractor}
import ru.poplavkov.foreader.wsd.WSDPreparator._
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

  def prepareWords(corpusDir: File, outDir: File, batchSize: Int = 2000): F[Unit] = {
    val files = corpusDir.listFiles
    val count = files.size
    files.toList.zipWithIndex.grouped(batchSize).toList
      .traverse(prepareBatch(_, count, outDir))
      .map(_ => ())
  }

  private def prepareBatch(batch: Seq[(File, Int)], count: Int, outDir: File): F[Unit] = {
    val itemsWithDocId = batch.toList.traverse { case (file, ind) =>
      val text = FileUtil.readFile(file.toPath)
      val fileId = file.getName
      for {
        _ <- Util.info[F](s"$ind/$count")
        sentences <- tokenExtractor.extractSentences(text)
        items <- lexicalItemExtractor.lexicalItemsFromSentences(sentences)
      } yield items.map(i => (i, fileId))
    }.map(_.flatten)

    for {
      items <- itemsWithDocId
      _ <- Util.info[F]("Grouping contexts")
      grouped = items.groupBy(_._1.qualifier).mapValues(_.map { case (item, docId) =>
        (item.context.get, docId)
      })
      _ <- Util.info[F]("Contexts grouped")
      _ <- grouped.toList.traverse { case (qualifier, contexts) =>
        flush(qualifier, contexts, outDir)
      }
      _ <- Util.info[F]("End processing of a batch")
    } yield ()
  }

  private def flush(qualifier: String,
                    contexts: Seq[(TextContext, DocId)],
                    outDir: File): F[Unit] = {
    val outFile = FileUtil.childFile(outDir, s"$qualifier.json")
    for {
      existing <- if (outFile.exists()) {
        Util.readJsonFile[F, Seq[(TextContext, String)]](outFile)
      } else {
        Seq.empty[(TextContext, String)].pure[F]
      }
      fullList = existing ++ contexts
      _ <- Util.writeToFileJson(outFile, fullList, readable = false)
    } yield ()
  }


}

object WSDPreparator {

  type DocId = String

}
