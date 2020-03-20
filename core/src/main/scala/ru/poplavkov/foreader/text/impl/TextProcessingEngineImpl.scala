package ru.poplavkov.foreader.text.impl

import cats.Monad
import cats.data.OptionT
import cats.instances.list._
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import ru.poplavkov.foreader._
import ru.poplavkov.foreader.dictionary.{Dictionary, DictionaryEntry}
import ru.poplavkov.foreader.text._
import ru.poplavkov.foreader.text.filter.{LexicalItemFilter, LexicalItemGroupFilter}

import scala.language.higherKinds

/**
  * @author mpoplavkov
  */
class TextProcessingEngineImpl[F[_] : Monad](tokenExtractor: TokenExtractor[F],
                                             lexicalItemExtractor: LexicalItemExtractor[F],
                                             lexicalItemFilter: LexicalItemFilter,
                                             levelDeterminator: LevelDeterminator[F],
                                             dictionary: Dictionary[F])
  extends TextProcessingEngine[F] {

  override def process(text: TextRepresentation[F], filter: LexicalItemGroupFilter): F[Seq[Card]] =
    for {
      allLexicalItems <- extractAllLexicalItems(text)
      filtered = lexicalItemFilter.filterItems(allLexicalItems)
      itemsWithDefOpt <- filtered.toList.traverse { item =>
        dictionary.getDefinition(item).value.map(entry => (item, entry))
      }
      itemsWithDef = itemsWithDefOpt.collect { case (item, Some(d)) => (item, d) }
      grouped = itemsWithDef.groupBy(_._2).mapValues(_.map(_._1))
      groups <- grouped.toList.traverse { case (entry, items) =>
        toLexicalItemGroup(entry, items).value
      }.map(_.flatten)
    } yield {
      filter.filterGroups(groups)
        .groupBy(_.items.head.lemmas)
        .map { case (lemmas, groups) =>
          Card(lemmas, groups.toSet)
        }
        .toSeq
    }

  private def toLexicalItemGroup(dictEntry: DictionaryEntry,
                                 items: Seq[LexicalItem]): OptionT[F, LexicalItemGroup] =
    for {
      level <- levelDeterminator.determineLevel(items.head).orElse(OptionT.pure(LexicalItemLevel.C2))
    } yield LexicalItemGroup(items, level, dictEntry)

  private def extractAllLexicalItems(text: TextRepresentation[F]): F[Seq[LexicalItem]] = {
    text.next.value.flatMap {
      case Some((value, nextTextRepr)) =>
        for {
          items <- itemsFromText(value)
          nextItems <- extractAllLexicalItems(nextTextRepr)
        } yield items ++ nextItems
      case None =>
        Seq.empty[LexicalItem].pure[F]
    }
  }

  private def itemsFromText(str: String): F[Seq[LexicalItem]] =
    for {
      tokens <- tokenExtractor.extract(str)
      items <- lexicalItemExtractor.lexicalItemsFromTokens(tokens)
    } yield items

}
