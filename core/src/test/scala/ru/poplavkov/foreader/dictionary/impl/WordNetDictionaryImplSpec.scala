package ru.poplavkov.foreader.dictionary.impl

import cats.effect.IO
import ru.poplavkov.foreader.Generators.generate
import ru.poplavkov.foreader.SpecBase
import ru.poplavkov.foreader.SpecBase._
import ru.poplavkov.foreader.dictionary.DictionaryEntry.Meaning
import ru.poplavkov.foreader.dictionary.{Dictionary, DictionaryEntry}
import ru.poplavkov.foreader.text.{LexicalItem, PartOfSpeech, Token}
import ru.poplavkov.foreader.SpecBase._
import ru.poplavkov.foreader.Generators._
import ru.poplavkov.foreader.vector.VectorsMap

/**
  * @author mpoplavkov
  */
class WordNetDictionaryImplSpec extends SpecBase {

  private val dictionary: Dictionary[IO] = new WordNetDictionaryImpl(VectorsMap.Empty, Map.empty)

  "WordnetDictionaryImpl" should {
    "extract dictionary entry for single word" in {
      val word = w"apple"

      val expectedMeaning1 = Meaning(
        id = d"apple%1:13:00::",
        definition = "fruit with red or yellow or green skin and sweet to tart crisp whitish flesh",
        partOfSpeech = Some(PartOfSpeech.Noun),
        examples = Seq.empty,
        synonyms = Seq.empty
      )
      val expectedMeaning2 = Meaning(
        id = d"apple%1:20:00::",
        definition = "native Eurasian tree widely cultivated in many varieties for its firm rounded edible fruits",
        partOfSpeech = Some(PartOfSpeech.Noun),
        examples = Seq.empty,
        synonyms = Seq("orchard apple tree", "Malus pumila")
      )

      val item = LexicalItem.SingleWord(generate[Token.Word].copy(lemma = word, partOfSpeech = PartOfSpeech.Noun))
      val expected = DictionaryEntry(Seq(expectedMeaning1, expectedMeaning2))

      dictionary.getDefinition(item).value.unsafeRunSync shouldBe Some(expected)
    }

    "extract dictionary entry for multi word expression" in {
      val words = Seq(w"post", w"office")

      val expectedMeaning1 = Meaning(
        id = d"post_office%1:14:01::",
        definition = "a local branch where postal services are available",
        partOfSpeech = Some(PartOfSpeech.Noun),
        examples = Seq.empty,
        synonyms = Seq("local post office")
      )
      val expectedMeaning2 = Meaning(
        id = d"post_office%1:14:00::",
        definition = "an independent agency of the federal government responsible for mail delivery (and sometimes " +
          "telecommunications) between individuals and businesses in the United States",
        partOfSpeech = Some(PartOfSpeech.Noun),
        examples = Seq.empty,
        synonyms = Seq("United States Post Office", "US Post Office", "PO")
      )
      val expectedMeaning3 = Meaning(
        id = d"post_office%1:04:00::",
        definition = "a children's game in which kisses are exchanged for pretended letters",
        partOfSpeech = Some(PartOfSpeech.Noun),
        examples = Seq.empty,
        synonyms = Seq.empty
      )

      val tokens = Seq(
        generate[Token.Word].copy(lemma = words.head, partOfSpeech = PartOfSpeech.Adjective),
        generate[Token.Word].copy(lemma = words(1), partOfSpeech = PartOfSpeech.Noun),
      )
      val item = LexicalItem.MultiWordExpression(tokens)

      val expected = DictionaryEntry(Seq(expectedMeaning1, expectedMeaning2, expectedMeaning3))

      dictionary.getDefinition(item).value.unsafeRunSync shouldBe Some(expected)
    }

    "extract dictionary entry with multiple examples" in {
      val word = w"book"

      val expectedMeaning1 = Meaning(
        id = d"book%2:31:00::",
        definition = "engage for a performance",
        partOfSpeech = Some(PartOfSpeech.Verb),
        examples = Seq("Her agent had booked her for several concerts in Tokyo"),
        synonyms = Seq.empty
      )
      val expectedMeaning2 = Meaning(
        id = d"book%2:41:01::",
        definition = "arrange for and reserve (something for someone else) in advance",
        partOfSpeech = Some(PartOfSpeech.Verb),
        examples = Seq(
          "reserve me a seat on a flight",
          "The agent booked tickets to the show for the whole family",
          "please hold a table at Maxim's"
        ),
        synonyms = Seq("reserve", "hold")
      )
      val expectedMeaning3 = Meaning(
        id = d"book%2:41:00::",
        definition = "record a charge in a police register",
        partOfSpeech = Some(PartOfSpeech.Verb),
        examples = Seq("The policeman booked her when she tried to solicit a man"),
        synonyms = Seq.empty
      )
      val expectedMeaning4 = Meaning(
        id = d"book%2:41:03::",
        definition = "register in a hotel booker",
        partOfSpeech = Some(PartOfSpeech.Verb),
        examples = Seq.empty,
        synonyms = Seq.empty
      )

      val item = LexicalItem.SingleWord(generate[Token.Word].copy(lemma = word, partOfSpeech = PartOfSpeech.Verb))
      val expected = DictionaryEntry(Seq(expectedMeaning1, expectedMeaning2, expectedMeaning3, expectedMeaning4))

      dictionary.getDefinition(item).value.unsafeRunSync shouldBe Some(expected)
    }

    "return None for nonexistent key" in {
      val word = w"nonexistentkey"
      val words = Seq(w"non", w"existent", w"key")
      val singleItem = LexicalItem.SingleWord(generate[Token.Word].copy(lemma = word))
      val tokens = Seq.tabulate(words.size)(i => generate[Token.Word].copy(lemma = words(i)))
      val multiItem = LexicalItem.MultiWordExpression(tokens)
      dictionary.getDefinition(singleItem).value.unsafeRunSync shouldBe None
      dictionary.getDefinition(multiItem).value.unsafeRunSync shouldBe None
    }
  }
}
