package ru.poplavkov.foreader.dictionary.impl

import cats.Id
import ru.poplavkov.foreader.Generators._
import ru.poplavkov.foreader.SpecBase._
import ru.poplavkov.foreader.dictionary.DictionaryEntry.Meaning
import ru.poplavkov.foreader.dictionary.{Dictionary, DictionaryEntry}
import ru.poplavkov.foreader.text.{LexicalItem, PartOfSpeech, Token}

/**
  * Spec for [[Dictionary]] based on the wordset dictionary
  *
  * @author mpoplavkov
  */
class WordsetDictionarySpec extends WordsetSpecBase[Id] {

  val dictionary: Dictionary[Id] = new DictionaryImpl[Id](extractor.extractDictionaryMap)

  "WordsetDictionary" should {

    "extract dictionary entry for single word" in {
      val word = w"lopsidedly"
      val expectedMeaning = Meaning(
        id = d"c2ab1fda0c",
        definition = "in a crooked lopsided manner",
        partOfSpeech = Some(PartOfSpeech.Adverb),
        examples = Seq("I smiled lopsidedly."),
        synonyms = Seq("crookedly")
      )

      val item = LexicalItem.SingleWord(generate[Token.Word].copy(lemma = word))
      val expected = DictionaryEntry(Seq(expectedMeaning))

      dictionary.getDefinition(item).value shouldBe Some(expected)

    }

    "extract dictionary entry for multi word expression" in {
      val words = Seq(w"a", w"day", w"late", w"and", w"a", w"dollar", w"short")
      val expectedMeaning = Meaning(
        id = d"ead341c72d",
        definition = "action taken too late and too feeble to be of any use",
        partOfSpeech = Some(PartOfSpeech.Noun),
        examples = Seq("The student's effort was a day late and a dollar short."),
        synonyms = Seq.empty
      )
      val tokens = Seq.tabulate(words.size)(i => generate[Token.Word].copy(lemma = words(i)))
      val item = LexicalItem.MultiWordExpression(tokens)
      val expected = DictionaryEntry(Seq(expectedMeaning))

      dictionary.getDefinition(item).value shouldBe Some(expected)

    }

    "extract dictionary entry without synonyms and examples" in {
      val word = w"lastingly"
      val expectedMeaning = Meaning(
        id = d"991677c80a",
        definition = "in an enduring or permanent manner",
        partOfSpeech = Some(PartOfSpeech.Adverb),
        examples = Seq.empty,
        synonyms = Seq.empty
      )
      val item = LexicalItem.SingleWord(generate[Token.Word].copy(lemma = word))
      val expected = DictionaryEntry(Seq(expectedMeaning))

      dictionary.getDefinition(item).value shouldBe Some(expected)

    }

    "extract dictionary entry with multiple meanings" in {
      val word = w"largely"
      val expectedMeaning1 = Meaning(
        id = d"9faec38240",
        definition = "in large part",
        partOfSpeech = Some(PartOfSpeech.Adverb),
        examples = Seq.empty,
        synonyms = Seq("mostly", "for the most part")
      )
      val expectedMeaning2 = Meaning(
        id = d"6801638f32",
        definition = "on a large scale",
        partOfSpeech = Some(PartOfSpeech.Adverb),
        examples = Seq("the sketch was so largely drawn that you could see it from the back row"),
        synonyms = Seq.empty
      )
      val item = LexicalItem.SingleWord(generate[Token.Word].copy(lemma = word))
      val expected = DictionaryEntry(Seq(expectedMeaning1, expectedMeaning2))

      dictionary.getDefinition(item).value shouldBe Some(expected)

    }

    "extract dictionary entry with multiple meanings with specified part of speech" in {
      val word = w"largely"
      val expectedMeaning1 = Meaning(
        id = d"9faec38240",
        definition = "in large part",
        partOfSpeech = Some(PartOfSpeech.Adverb),
        examples = Seq.empty,
        synonyms = Seq("mostly", "for the most part")
      )
      val expectedMeaning2 = Meaning(
        id = d"6801638f32",
        definition = "on a large scale",
        partOfSpeech = Some(PartOfSpeech.Adverb),
        examples = Seq("the sketch was so largely drawn that you could see it from the back row"),
        synonyms = Seq.empty
      )
      val item = LexicalItem.SingleWord(generate[Token.Word].copy(lemma = word, partOfSpeech = PartOfSpeech.Adverb))
      val expected = DictionaryEntry(Seq(expectedMeaning1, expectedMeaning2))

      dictionary.getDefinition(item).value shouldBe Some(expected)

    }

    "extract dictionary entry with only specified part of speech" in {
      val word = w"about"
      val expectedMeaning = Meaning(
        id = d"2edf9a8823",
        definition = "on the move",
        partOfSpeech = Some(PartOfSpeech.Adjective),
        examples = Seq.empty,
        synonyms = Seq("astir")
      )
      val item = LexicalItem.SingleWord(generate[Token.Word].copy(lemma = word, partOfSpeech = PartOfSpeech.Adjective))
      val expected = DictionaryEntry(Seq(expectedMeaning))

      dictionary.getDefinition(item).value shouldBe Some(expected)

    }

    "return None for nonexistent key" in {
      val word = w"nonexistentkey"
      val words = Seq(w"non", w"existent", w"key")
      val singleItem = LexicalItem.SingleWord(generate[Token.Word].copy(lemma = word))
      val tokens = Seq.tabulate(words.size)(i => generate[Token.Word].copy(lemma = words(i)))
      val multiItem = LexicalItem.MultiWordExpression(tokens)
      dictionary.getDefinition(singleItem).value shouldBe None
      dictionary.getDefinition(multiItem).value shouldBe None
    }

  }

}
