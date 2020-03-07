package ru.poplavkov.foreader.text.impl

import cats.Monad
import cats.data.OptionT
import cats.instances.list._
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import ru.poplavkov.foreader.Globals.WordStr
import ru.poplavkov.foreader.LexicalItem
import ru.poplavkov.foreader.dictionary.Dictionary
import ru.poplavkov.foreader.text._

import scala.language.higherKinds

/**
  * @author mpoplavkov
  */
class TextProcessingEngineImpl[F[_] : Monad](tokenExtractor: TokenExtractor[F],
                                             lexicalItemExtractor: LexicalItemExtractor[F],
                                             levelDeterminator: LevelDeterminator[F],
                                             dictionary: Dictionary[F])
  extends TextProcessingEngine[F] {

  override def process(text: TextRepresentation[F], filter: LexicalItemGroupFilter): F[Seq[Card]] =
    for {
      allLexicalItems <- extractAllLexicalItems(text)
      grouped = allLexicalItems.groupBy(item => (item.partsOfSpeech, item.lemmas))
      groups <- grouped.toList.traverse { case ((pos, lemmas), items) =>
        toLexicalItemGroup(items, pos, lemmas).value
      }.map(_.flatten)
    } yield {
      filter.filterGroups(groups)
        .groupBy(_.items.head.lemmas)
        .map { case (lemmas, groups) =>
          Card(lemmas, groups.toSet)
        }
        .toSeq
    }

  // TODO: filter definitions by part of speech
  private def toLexicalItemGroup(items: Seq[LexicalItem],
                                 pos: Seq[PartOfSpeech],
                                 lemmas: Seq[WordStr]): OptionT[F, LexicalItemGroup] =
    for {
      level <- levelDeterminator.determineLevel(items.head).orElse(OptionT.pure(LexicalItemLevel.C2))
      definition <- dictionary.getDefinition(lemmas)
    } yield LexicalItemGroup(items, level, definition)

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
