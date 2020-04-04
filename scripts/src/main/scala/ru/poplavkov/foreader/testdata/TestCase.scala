package ru.poplavkov.foreader.testdata

import ru.poplavkov.foreader.dictionary.DictionaryEntry
import ru.poplavkov.foreader.text.Token

/**
  * @author mpoplavkov
  */
case class TestCase(sentence: Seq[Token],
                    word: Token.Word,
                    meanings: Seq[DictionaryEntry.Meaning])
