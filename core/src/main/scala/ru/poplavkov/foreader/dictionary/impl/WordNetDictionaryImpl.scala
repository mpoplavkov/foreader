package ru.poplavkov.foreader.dictionary.impl

import cats.data.OptionT
import cats.effect.Sync
import com.softwaremill.tagging._
import net.sf.extjwnl.data.POS
import net.sf.extjwnl.dictionary.{Dictionary => WordNetDictionary}
import ru.poplavkov.foreader.Globals.DictionaryMeaningIdTag
import ru.poplavkov.foreader.dictionary.impl.WordNetDictionaryImpl._
import ru.poplavkov.foreader.dictionary.{Dictionary, DictionaryEntry}
import ru.poplavkov.foreader.text.{LexicalItem, PartOfSpeech}

import scala.collection.JavaConverters._
import scala.language.higherKinds

/**
  * @author mpoplavkov
  */
class WordNetDictionaryImpl[F[_] : Sync] extends Dictionary[F] {

  private val dict: WordNetDictionary = WordNetDictionary.getDefaultResourceInstance

  override def getDefinition(lexicalItem: LexicalItem): OptionT[F, DictionaryEntry] =
    OptionT(Sync[F].delay {
      lexicalItem match {
        case LexicalItem.SingleWord(word, _) =>
          entryFromDictionary(word.partOfSpeech, word.lemma)
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

