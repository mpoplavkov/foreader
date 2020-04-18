package ru.poplavkov.foreader.text.impl

import cats.Monad
import cats.instances.list._
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import ru.poplavkov.foreader.Globals.WordStr
import ru.poplavkov.foreader.dictionary.MweSet
import ru.poplavkov.foreader.text.impl.LexicalItemExtractorImpl._
import ru.poplavkov.foreader.text.{ContextExtractor, LexicalItem, LexicalItemExtractor, Token}

import scala.annotation.tailrec
import scala.language.higherKinds

/**
  * @author mpoplavkov
  */
class LexicalItemExtractorImpl[F[+_] : Monad](mweSet: MweSet[F])
  extends LexicalItemExtractor[F] {

  override def lexicalItemsFromSentences(tokensBySentences: Seq[Seq[Token]]): F[Seq[LexicalItem]] = {
    tokensBySentences.toList.traverse { sentence =>
      tokensToLexicalItemsInternal(sentence.toList).map { items =>
        items.map(item => item.setContext(Some(ContextExtractor(sentence, item.wordTokens))))
      }
    }.map(_.flatten)
  }

  // TODO: check if it would not fail with StackOverflow for huge inputs
  private def tokensToLexicalItemsInternal(sentenceTokens: List[Token],
                                           resultReversed: List[LexicalItem] = Nil): F[Seq[LexicalItem]] =
    sentenceTokens match {
      case (word: Token.Word) :: rest =>
        mweSet.getMwesStartingWith(word.lemma).flatMap { set =>
          val (newItem, newRest) =
            firstFoundMweAndRest(rest, set) match {
              case Some((mwe, newRest)) =>
                (LexicalItem.MultiWordExpression(word :: mwe), newRest)
              case None =>
                (LexicalItem.SingleWord(word), rest)
            }
          tokensToLexicalItemsInternal(newRest, newItem :: resultReversed)
        }
      case _ :: rest =>
        tokensToLexicalItemsInternal(rest, resultReversed)
      case Nil =>
        resultReversed.reverse.pure[F]
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

  // TODO: think about this method, since tokens are the sentence
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
