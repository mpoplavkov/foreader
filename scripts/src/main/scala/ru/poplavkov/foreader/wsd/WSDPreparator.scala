package ru.poplavkov.foreader.wsd

import java.io.File
import java.time.Instant
import java.util.Base64

import cats.effect.Sync
import cats.instances.list._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import com.softwaremill.tagging._
import io.circe.generic.auto._
import ru.poplavkov.foreader.Globals._
import ru.poplavkov.foreader.collocation.{Classifier, OneItemClassifier, WordCollocation}
import ru.poplavkov.foreader.dictionary.{Dictionary, DictionaryEntry}
import ru.poplavkov.foreader.text.{LexicalItem, LexicalItemExtractor, TextContext, TokenExtractor}
import ru.poplavkov.foreader.{FileUtil, _}

import scala.annotation.tailrec
import scala.language.higherKinds

/**
  * Prepares model for word sense disambiguation
  *
  * @author mpoplavkov
  */
class WSDPreparator[F[_] : Sync](tokenExtractor: TokenExtractor[F],
                                 lexicalItemExtractor: LexicalItemExtractor[F],
                                 dictionary: Dictionary[F],
                                 k: Int) {

  def prepareWords(corpusDir: File, outDir: File, batchSize: Int = 2000): F[Unit] = {
    val files = corpusDir.listFiles
    val count = files.size
    for {
      _ <- files.toList.zipWithIndex.grouped(batchSize).toList
        .traverse(prepareBatch(_, count, outDir))
      _ <- combineWordFiles(outDir)
    } yield ()
  }

  private def prepareBatch(batch: Seq[(File, Int)], count: Int, outDir: File): F[Unit] = {
    val itemsWithDocId = batch.toList.traverse { case (file, ind) =>
      val text = FileUtil.readFile(file.toPath)
      val fileId = file.getName
      for {
        _ <- Util.info[F](s"$ind/$count")
        items <- lexicalItemsFrom(text)
        filtered <- items.toList.traverse { item =>
          dictionary.getDefinition(item).value.map(_.map(_ => item))
        }.map(_.flatten)
      } yield filtered.map(i => (i, fileId))
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

  private def flush(qualifier: Qualifier,
                    contexts: Seq[(TextContext, DocId)],
                    outDir: File): F[Unit] = {
    val encodedQualifier = Base64.getEncoder.encodeToString(qualifier.getBytes)
    val wordDir = FileUtil.childFile(outDir, encodedQualifier)
    wordDir.mkdir()
    val outFile = FileUtil.childFile(wordDir, s"${Instant.now().toString}.json")
    Util.writeToFileJson(outFile, contexts, readable = false)
  }

  private def combineWordFiles(contextsDir: File): F[Unit] = {
    contextsDir.listFiles.toList.traverse { wordDir =>
      val outFile = FileUtil.childFile(contextsDir, s"${wordDir.getName}.json")
      wordDir.listFiles.toList
        .traverse { file =>
          Util.readJsonFile[F, Seq[(TextContext, DocId)]](file).map { contexts =>
            file.delete()
            contexts
          }
        }
        .map(_.flatten)
        .flatMap { fullList =>
          wordDir.delete()
          Util.writeToFileJson(outFile, fullList, readable = false)
        }
    }.map(_ => ())
  }

  def calculateClassifier(contextsDir: File,
                          propagateToTheWholeDoc: Boolean,
                          iterations: Int,
                          outDir: File): F[Unit] = {
    val classifierFile = FileUtil.childFile(outDir, "classifier.json")
    val oneItemDir = FileUtil.childFile(outDir, "one_item")
    oneItemDir.mkdir()

    val calcItemClassifiers: F[Unit] = contextsDir.listFiles.toList
      .traverse { file =>
        for {
          classifier <- calculateClassifierForItem(file, propagateToTheWholeDoc, iterations)
          oneItemFile = FileUtil.childFile(oneItemDir, file.getName)
          _ <- Util.writeToFileJson(oneItemFile, classifier, readable = false)
        } yield ()
      }.map(_ => ())

    for {
      _ <- calcItemClassifiers
      itemClasssifiers <- oneItemDir.listFiles.toList.traverse { file =>
        Util.readJsonFile[F, OneItemClassifier](file)
          .map(cl => qualifierFromFileName(file.getName) -> cl)
      }

      classifier = Classifier(itemClasssifiers.toMap)
      _ <- Util.writeToFileJson(classifierFile, classifier)
    } yield ()

  }

  private def calculateClassifierForItem(wordContextsFile: File,
                                         propagateToTheWholeDoc: Boolean,
                                         iterations: Int): F[OneItemClassifier] = {
    val qualifier = qualifierFromFileName(wordContextsFile.getName)
    for {
      contexts <- Util.readJsonFile[F, Seq[(TextContext, DocId)]](wordContextsFile)
      trainData <- initialTrainData(qualifier, contexts)
      classifier <- Sync[F].delay(train(trainData, propagateToTheWholeDoc, iterations))
    } yield classifier
  }

  @tailrec
  private def train(trainData: Seq[TrainEntry],
                    propagateToTheWholeDoc: Boolean, iterations: Int): OneItemClassifier = {
    val classifier = trainDataToClassifier(trainData)
    if (iterations == 0) {
      classifier
    } else {
      val newTrained = trainData.map { case TrainEntry(collocations, _, docId) =>
        TrainEntry(collocations, classifier(collocations), docId)
      }
      val propagated = if (propagateToTheWholeDoc) {
        val docToMeaning = newTrained.groupBy(_.docId)
          .mapValues(_.flatMap(_.meaningId))
          .mapValues(meanings => CollectionUtil.mostFrequentElement(meanings)._1)
        newTrained.map { case TrainEntry(collocations, meaningOpt, docId) =>
          val meaning = meaningOpt.orElse(docToMeaning.get(docId))
          TrainEntry(collocations, meaning, docId)
        }
      } else {
        newTrained
      }

      train(propagated, propagateToTheWholeDoc, iterations - 1)
    }
  }

  private def initialTrainData(qualifier: Qualifier, contexts: Seq[(TextContext, DocId)]): F[Seq[TrainEntry]] = {
    for {
      meanings <- meaningsByQualifier(qualifier)
      normalizedMeanings <- meanings.toList.traverse(normalizeMeaning)

    } yield contexts.map { case (context, docId) =>
      val meaningIdOpt = meaningIdByIntersectionWithContext(normalizedMeanings, context)
      val collocations = WordCollocation.fromContext(context, k)
      TrainEntry(collocations, meaningIdOpt, docId)
    }

  }

  private def meaningsByQualifier(qualifier: Qualifier): F[Seq[DictionaryEntry.Meaning]] = {
    val entryOpt = LexicalItem.parseQualifier(qualifier) match {
      case Right((word, pos)) => dictionary.getDefinition(word, pos)
      case Left(mwe) => dictionary.getDefinition(mwe)
    }
    entryOpt.value.map(_.toSeq.flatMap(_.meanings))
  }

  private def normalizeMeaning(meaning: DictionaryEntry.Meaning): F[(DictionaryMeaningId, Seq[WordStr])] =
    for {
      items <- lexicalItemsFrom(meaning.definition)
      normalized = items.flatMap(_.lemmas)
    } yield (meaning.id, normalized)

  private def meaningIdByIntersectionWithContext(meanings: Seq[(DictionaryMeaningId, Seq[WordStr])],
                                                 context: TextContext): Option[DictionaryMeaningId] = {
    require(meanings.nonEmpty)
    val contextWords = context match {
      case sw: TextContext.SurroundingWords => sw.allWords
    }
    val (bestId, bestCount) = meanings.map { case (id, words) =>
      id -> words.count(contextWords.contains)
    }.maxBy(_._2)
    if (bestCount == 0) {
      None
    } else {
      Some(bestId)
    }
  }

  private def lexicalItemsFrom(str: String): F[Seq[LexicalItem]] =
    for {
      sentences <- tokenExtractor.extractSentences(str)
      items <- lexicalItemExtractor.lexicalItemsFromSentences(sentences)
    } yield items

  private def trainDataToClassifier(trainData: Seq[TrainEntry]): OneItemClassifier = {
    val facts = trainData.collect { case TrainEntry(collocations, Some(meaningId), _) =>
      collocations -> meaningId
    }
    OneItemClassifier.fromFacts(facts)
  }

  private def qualifierFromFileName(fileName: String): Qualifier = {
    val encodedQualifier = fileName.replace(".json", "")
    new String(Base64.getDecoder.decode(encodedQualifier)).taggedWith[QualifierTag]
  }

}
