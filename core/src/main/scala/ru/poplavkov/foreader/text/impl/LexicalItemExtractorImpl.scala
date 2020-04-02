package ru.poplavkov.foreader.text.impl

import cats.Monad
import cats.syntax.applicative._
import cats.syntax.flatMap._
import ru.poplavkov.foreader.Globals.WordStr
import ru.poplavkov.foreader.dictionary.MweSet
import ru.poplavkov.foreader.text.impl.LexicalItemExtractorImpl._
import ru.poplavkov.foreader.text.{ContextExtractor, LexicalItem, LexicalItemExtractor, TextContext, Token}

import scala.annotation.tailrec
import scala.language.higherKinds

/**
  * @author mpoplavkov
  */
class LexicalItemExtractorImpl[F[+ _] : Monad](mweSet: MweSet[F], contextExtractor: ContextExtractor)
  extends LexicalItemExtractor[F] {

  override def lexicalItemsFromTokens(tokens: Seq[Token]): F[Seq[LexicalItem]] =
    tokensToLexicalItemsInternal(tokens.toList)

  // TODO: check if it would not fail with StackOverflow for huge inputs
  private def tokensToLexicalItemsInternal(tokens: List[Token],
                                           resultReversed: List[LexicalItem] = Nil,
                                           tokensBeforeReversed: List[Token] = Nil): F[Seq[LexicalItem]] = {

    def ctxt: Seq[Token] => TextContext = contextExtractor.extractContext(tokensBeforeReversed, _)

    tokens match {
      case (word: Token.Word) :: rest =>
        mweSet.getMwesStartingWith(word.lemma).flatMap { set =>
          firstFoundMweAndRest(rest, set) match {
            case Some((mwe, newRest)) =>
              val item = LexicalItem.MultiWordExpression(word :: mwe, ctxt(newRest))
              tokensToLexicalItemsInternal(
                newRest,
                item :: resultReversed,
                word :: tokensBeforeReversed
              )
            case None =>
              tokensToLexicalItemsInternal(
                rest,
                LexicalItem.SingleWord(word, ctxt(rest)) :: resultReversed,
                word :: tokensBeforeReversed
              )
          }
        }
      case head :: rest =>
        tokensToLexicalItemsInternal(rest, resultReversed, head :: tokensBeforeReversed)
      case Nil =>
        resultReversed.reverse.pure[F]
    }
  }
}

object LexicalItemExtractorImpl {

  private def firstFoundMweAndRest(tokens: List[Token],
                                   mweSet: Set[Seq[WordStr]]): Option[(List[Token.Word], List[Token])] = {
    lazy val (words, restTokens) = extractStartingWords(tokens, limit = 10)
    mweSet.foldLeft[Option[(List[Token.Word], List[Token])]](None) {
      case (None, mwe) =>
        removeSubsequenceInOrder(words, mwe.toList).map { case (mweFound, withoutMwe) =>
          (mweFound, withoutMwe ++ restTokens)
        }
      case (some, _) =>
        some
    }
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
                                   limit: Int,
                                   alreadyExtracted: List[Token.Word] = Nil): (List[Token.Word], List[Token]) =
    tokens match {
      case (word: Token.Word) :: rest if limit > 0 =>
        extractStartingWords(rest, limit - 1, alreadyExtracted :+ word)
      case _ =>
        (alreadyExtracted, tokens)
    }

}
