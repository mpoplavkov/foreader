package ru.poplavkov.foreader.text.impl

import cats.Monad
import cats.syntax.applicative._
import cats.syntax.flatMap._
import ru.poplavkov.foreader.Globals.WordStr
import ru.poplavkov.foreader.LexicalItem
import ru.poplavkov.foreader.dictionary.MweSet
import ru.poplavkov.foreader.text.{LexicalItemExtractor, Token, TokensFilter}

import scala.annotation.tailrec

/**
  * @author mpoplavkov
  */
class LexicalItemExtractorImpl[F[+_] : Monad](filter: TokensFilter,
                                             mweSet: MweSet[F])
  extends LexicalItemExtractor[F] {

  override def lexicalItemsFromTokens(tokens: Seq[Token]): F[Seq[LexicalItem]] =
    tokensToLexicalItemsInternal(tokens.toList)

  // TODO: check if it would not fail with StackOverflow for huge inputs
  private def tokensToLexicalItemsInternal(tokens: List[Token],
                                           resultReversed: List[LexicalItem] = Nil): F[Seq[LexicalItem]] =
    tokens match {
      case (word: Token.Word) :: rest if filter.filter(word) =>
        mweSet.getMwesStartingWith(word.lemma).flatMap {
          case set if set.isEmpty =>
            tokensToLexicalItemsInternal(rest, LexicalItem.SingleWord(word) :: resultReversed)
          case set =>
            val (words, restTokens) = extractStartingWords(rest)
            val firstFoundMwe = set.foldLeft[Option[(List[Token.Word], List[Token.Word])]](None) {
              case (None, mwe) =>
                removeSubsequenceInOrder(words, mwe.toList)
              case (some, _) =>
                some
            }

            firstFoundMwe match {
              case Some((mwe, restOfTheWords)) =>
                tokensToLexicalItemsInternal(restOfTheWords ++ restTokens, LexicalItem.MultiWordExpression(mwe) :: resultReversed)
              case None =>
                tokensToLexicalItemsInternal(rest, LexicalItem.SingleWord(word) :: resultReversed)
            }
        }
      case _ :: rest =>
        tokensToLexicalItemsInternal(rest, resultReversed)
      case Nil =>
        resultReversed.reverse.pure[F]
    }

  /**
    * Removes all elements from the `list` presented in `toRemove` in the same
    * relative order that was in `toRemove`
    *
    * @return removed elements and the same list without them if all elements
    *         from `toRemove` were present in the initial list, None otherwise
    */
  @tailrec
  private def removeSubsequenceInOrder(list: List[Token.Word],
                                       toRemove: List[WordStr],
                                       removed: List[Token.Word] = Nil,
                                       rest: List[Token.Word] = Nil): Option[(List[Token.Word], List[Token.Word])] = {
    (list, toRemove) match {
      case (_, Nil) =>
        Some((removed, rest ++ list))
      case (Nil, _) =>
        None
      case (head1 :: tail1, head2 :: tail2) if head1.lemma == head2 =>
        removeSubsequenceInOrder(tail1, tail2, removed :+ head1, rest)
      case (head :: tail, remove) =>
        removeSubsequenceInOrder(tail, remove, removed, rest :+ head)
    }
  }

  /**
    * Extracts list of words from the beginning of the given `tokens`
    * till the first punctuation mark or the end of tokens
    */
  @tailrec
  private def extractStartingWords(tokens: List[Token],
                                   alreadyExtracted: List[Token.Word] = Nil): (List[Token.Word], List[Token]) =
    tokens match {
      case (word: Token.Word) :: rest => extractStartingWords(rest, alreadyExtracted :+ word)
      case _ => (alreadyExtracted, tokens)
    }

}
