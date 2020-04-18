package ru.poplavkov.foreader.text.impl

import java.util.Properties

import cats.effect.Sync
import com.softwaremill.tagging._
import edu.stanford.nlp.pipeline.{CoreDocument, CoreSentence, StanfordCoreNLP}
import ru.poplavkov.foreader.Globals.WordStrTag
import ru.poplavkov.foreader.Language
import ru.poplavkov.foreader.text.impl.CoreNlpTokenExtractor._
import ru.poplavkov.foreader.text.{PosTag, Token, TokenExtractor}

import scala.collection.JavaConverters._
import scala.language.higherKinds

/**
  * [[TokenExtractor]] based on the Stanford CoreNLP library
  *
  * @see https://stanfordnlp.github.io/CoreNLP/
  * @author mpoplavkov
  */
class CoreNlpTokenExtractor[F[_] : Sync](language: Language) extends TokenExtractor[F] {

  private val pipeline = createCoreNlpPipeline(language)

  override def extractSentences(text: String): F[Seq[Seq[Token]]] = Sync[F].delay {
    val document = new CoreDocument(text)
    pipeline.annotate(document)

    document.sentences().asScala.map { sentence =>
      extractTokensFromSentence(sentence)
    }

  }
}

object CoreNlpTokenExtractor {

  private def createCoreNlpPipeline(language: Language): StanfordCoreNLP = {
    val lang = langString(language)
    val props = new Properties

    // set the list of annotators to run
    props.setProperty("annotators", "tokenize,ssplit,pos,lemma")
    // set a property for an annotator
    props.setProperty("coref.algorithm", "neural")

    props.setProperty("tokenize.language", lang)
    props.setProperty("coref.language", lang)

    new StanfordCoreNLP(props)
  }

  private def langString(language: Language): String =
    language match {
      case Language.English => "en"
    }

  private def extractTokensFromSentence(sentence: CoreSentence): Seq[Token] = {
    val tokens = sentence.tokens().asScala
    val partOfSpeechTags = sentence.posTags().asScala

    tokens.zip(partOfSpeechTags).map { case (token, tag) =>
      val lemma = token.lemma.toLowerCase
      val posTag = new PosTag(lemma, tag)
      posTag.asPartOfSpeechOrPunc match {
        case Right(partOfSpeech) =>
          Token.Word(
            position = token.beginPosition,
            original = token.originalText.taggedWith[WordStrTag],
            lemma = lemma.taggedWith[WordStrTag],
            partOfSpeech = partOfSpeech
          )
        case Left(punctuationMark) =>
          Token.Punctuation(
            position = token.beginPosition,
            mark = punctuationMark
          )
      }
    }
  }

}
