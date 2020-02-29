package ru.poplavkov.foreader.dictionary.wordset

import cats.Id
import com.softwaremill.tagging._
import ru.poplavkov.foreader.Globals.{WordStr, WordStrTag}
import ru.poplavkov.foreader.SpecBase._
import ru.poplavkov.foreader.dictionary.DictionaryEntry.Meaning
import ru.poplavkov.foreader.dictionary.impl.MapDictionaryImpl
import ru.poplavkov.foreader.dictionary.{Dictionary, DictionaryEntry}
import ru.poplavkov.foreader.text.PartOfSpeech

/**
  * Spec for [[Dictionary]] based on the wordset dictionary
  *
  * @author mpoplavkov
  */
class WordsetDictionarySpec extends WordsetSpecBase[Id] {

  val dictionary: Dictionary[Id] = new MapDictionaryImpl[Id](extractor.extractDictionaryMap)

  "WordsetDictionary" should {

    "extract dictionary entry for single word" in {
      val word = w"lopsidedly"
      val expectedMeaning = Meaning(
        definition = "in a crooked lopsided manner",
        partOfSpeech = Some(PartOfSpeech.Adverb),
        examples = Seq("I smiled lopsidedly."),
        synonyms = Seq("crookedly")
      )
      val expected = DictionaryEntry(Seq(expectedMeaning))

      dictionary.getDefinition(word).value shouldBe Some(expected)

    }

    "extract dictionary entry for multi word expression" in {
      val words = Seq(w"a", w"day", w"late", w"and", w"a", w"dollar", w"short")
      val expectedMeaning = Meaning(
        definition = "action taken too late and too feeble to be of any use",
        partOfSpeech = Some(PartOfSpeech.Noun),
        examples = Seq("The student's effort was a day late and a dollar short."),
        synonyms = Seq.empty
      )
      val expected = DictionaryEntry(Seq(expectedMeaning))

      dictionary.getDefinition(words).value shouldBe Some(expected)

    }

    "extract dictionary entry without synonyms and examples" in {
      val word = w"lastingly"
      val expectedMeaning = Meaning(
        definition = "in an enduring or permanent manner",
        partOfSpeech = Some(PartOfSpeech.Adverb),
        examples = Seq.empty,
        synonyms = Seq.empty
      )
      val expected = DictionaryEntry(Seq(expectedMeaning))

      dictionary.getDefinition(word).value shouldBe Some(expected)

    }

    "extract dictionary entry with multiple meanings" in {
      val word = w"largely"
      val expectedMeaning1 = Meaning(
        definition = "in large part",
        partOfSpeech = Some(PartOfSpeech.Adverb),
        examples = Seq.empty,
        synonyms = Seq("mostly", "for the most part")
      )
      val expectedMeaning2 = Meaning(
        definition = "on a large scale",
        partOfSpeech = Some(PartOfSpeech.Adverb),
        examples = Seq("the sketch was so largely drawn that you could see it from the back row"),
        synonyms = Seq.empty
      )
      val expected = DictionaryEntry(Seq(expectedMeaning1, expectedMeaning2))

      dictionary.getDefinition(word).value shouldBe Some(expected)

    }

    "return None for MWE as one string" in {
      val word = w"a day late and a dollar short"
      val existedWords = word.split(" ").map(_.taggedWith[WordStrTag])
      dictionary.getDefinition(existedWords).value shouldBe 'nonEmpty
      dictionary.getDefinition(word).value shouldBe None
    }

    "return None for nonexistent key" in {
      val word = w"nonexistent"
      val words = Seq(w"non", w"existent")
      dictionary.getDefinition(word).value shouldBe None
      dictionary.getDefinition(words).value shouldBe None
    }

  }

}
