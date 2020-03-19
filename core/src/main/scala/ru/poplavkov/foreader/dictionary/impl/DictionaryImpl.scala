package ru.poplavkov.foreader.dictionary.impl

import cats.Applicative
import cats.data.OptionT
import com.softwaremill.tagging._
import ru.poplavkov.foreader.Globals.{WordStr, WordStrTag}
import ru.poplavkov.foreader.VectorUtil
import ru.poplavkov.foreader.dictionary.{Dictionary, DictionaryEntry, DictionaryMap}
import ru.poplavkov.foreader.text.{LexicalItem, PartOfSpeech, TextContext}
import ru.poplavkov.foreader.vector.{MathVector, VectorsMap}

import scala.language.higherKinds

/**
  * Implementation of the [[Dictionary]] based on the [[DictionaryMap]]
  *
  * @author mpoplavkov
  */
class DictionaryImpl[F[_] : Applicative](map: DictionaryMap,
                                         vectors: VectorsMap) extends Dictionary[F] {

  override def getDefinition(lexicalItem: LexicalItem): OptionT[F, DictionaryEntry] =
    map.get(lexicalItem.lemmas) match {
      case Some(entry) =>
        OptionT.some(filterMeanings(entry, lexicalItem.partsOfSpeech.headOption, lexicalItem.context))
      case None =>
        OptionT.none
    }

  private def filterMeanings(entry: DictionaryEntry,
                             pos: Option[PartOfSpeech], // TODO: get rid of the Option
                             context: TextContext): DictionaryEntry = {
    val filteredByPos = entry.filterMeanings(m => pos.forall(m.partOfSpeech.contains))
    val nonEmpty = if (filteredByPos.isEmpty) entry else filteredByPos
    if (nonEmpty.meanings.size > 1) {
      val contextVector = context2Vector(context)
      val meaning = nonEmpty.meanings.minBy { m =>
        val words = m.definition.split(" ").map(_.taggedWith[WordStrTag])
        val avgDefVector = avgVectorForWords(words)
        VectorUtil.euclideanDistance(contextVector, avgDefVector)
      }
      DictionaryEntry.withOneMeaning(meaning)
    } else {
      nonEmpty
    }
  }

  private def context2Vector(context: TextContext): MathVector = context match {
    case TextContext.Empty =>
      MathVector.zero(vectors.dimension)
    case ctx: TextContext.SurroundingWords =>
      avgVectorForWords(ctx.allWords)
  }

  private def avgVectorForWords(words: Seq[WordStr]): MathVector = {
    val wordVectors = words.map(_.toLowerCase.taggedWith).flatMap(vectors.getVector)
    VectorUtil.avgVector(vectors.dimension, wordVectors)
  }

}
