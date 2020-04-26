package ru.poplavkov.foreader.dictionary.impl

import cats.data.OptionT
import cats.effect.Sync
import com.softwaremill.tagging._
import net.sf.extjwnl.data.POS
import net.sf.extjwnl.dictionary.{Dictionary => WordNetDictionary}
import ru.poplavkov.foreader.Globals.{DictionaryMeaningId, DictionaryMeaningIdTag}
import ru.poplavkov.foreader.VectorUtil
import ru.poplavkov.foreader.dictionary.impl.WordNetDictionaryImpl._
import ru.poplavkov.foreader.dictionary.{Dictionary, DictionaryEntry}
import ru.poplavkov.foreader.text.{LexicalItem, PartOfSpeech, TextContext}
import ru.poplavkov.foreader.vector.{MathVector, VectorsMap}

import scala.collection.JavaConverters._
import scala.language.higherKinds

/**
  * @author mpoplavkov
  */
class WordNetDictionaryImpl[F[_] : Sync](vectorsMap: VectorsMap,
                                         meaningToContext: Map[DictionaryMeaningId, MathVector])
  extends Dictionary[F] {

  private val dict: WordNetDictionary = WordNetDictionary.getDefaultResourceInstance

  override def getDefinition(lexicalItem: LexicalItem): OptionT[F, DictionaryEntry] =
    OptionT(Sync[F].delay {
      lexicalItem match {
        case LexicalItem.SingleWord(word, context) =>
          entryFromDictionary(word.partOfSpeech, word.lemma)
            .map(orderMeaningsByContext(_, context))
        case LexicalItem.MultiWordExpression(words, _) =>
          PartOfSpeech.all.toStream
            .flatMap(entryFromDictionary(_, words.map(_.lemma).mkString(" ")))
            .headOption
      }
    })

  private def entryFromDictionary(partOfSpeech: PartOfSpeech, lemma: String): Option[DictionaryEntry] = {
    val meanings = for {
      pos <- partOfSpeech2POS(partOfSpeech).toSeq
      indexWord <- Option(dict.getIndexWord(pos, lemma)).toSeq
      _ = indexWord.sortSenses()
      sense <- indexWord.getSenses.asScala
      glossary = sense.getGloss
      defsAndExamples = glossary.split("; ")
      (examplesWithQuotes, defs) = defsAndExamples.partition(_.startsWith("\""))
      examples = examplesWithQuotes.map(_.replaceAll("\"", ""))
      synsetWords = sense.getWords.asScala
      (sameWord, synonyms) = synsetWords.partition(_.getLemma.equalsIgnoreCase(lemma))
      senseKey <- sameWord.headOption.map(_.getSenseKey)
    } yield
      DictionaryEntry.Meaning(
        id = senseKey.taggedWith[DictionaryMeaningIdTag],
        definition = defs.mkString("; "),
        partOfSpeech = Some(partOfSpeech),
        examples = examples,
        synonyms = synonyms.map(_.getLemma)
      )

    if (meanings.nonEmpty) {
      Some(DictionaryEntry(meanings))
    } else {
      None
    }
  }

  private def orderMeaningsByContext(entry: DictionaryEntry,
                                     context: Option[TextContext]): DictionaryEntry =
    contextToVector(context) match {
      case Some(contextVector) =>
        val meaningsToDistance = entry.meanings.map { meaning =>
          val distanceOpt = meaningToContext.get(meaning.id).map(VectorUtil.euclideanDistance(_, contextVector))
          meaning -> distanceOpt
        }

        val firstOrder = meaningsToDistance.collect {
          case (meaning, Some(distance)) =>
            meaning -> distance
        }.sortBy(_._2).map(_._1)

        val secondOrder = meaningsToDistance.collect {
          case (meaning, None) => meaning
        }

        val ordered = firstOrder ++ secondOrder
        entry.copy(meanings = ordered)
      case None =>
        entry
    }

  private def contextToVector(context: Option[TextContext]): Option[MathVector] =
    context match {
      case Some(TextContext.SurroundingWords(before, after)) =>
        val vectors = (before ++ after).flatMap(vectorsMap.getVector)
        if (vectors.nonEmpty) {
          Some(VectorUtil.avgVector(vectorsMap.dimension, vectors))
        } else {
          None
        }
      case None =>
        None
    }

}

object WordNetDictionaryImpl {

  private def partOfSpeech2POS(partOfSpeech: PartOfSpeech): Option[POS] =
    partOfSpeech match {
      case PartOfSpeech.Noun => Some(POS.NOUN)
      case PartOfSpeech.Verb => Some(POS.VERB)
      case PartOfSpeech.Adjective => Some(POS.ADJECTIVE)
      case PartOfSpeech.Adverb => Some(POS.ADVERB)
      case _ => None
    }

}

