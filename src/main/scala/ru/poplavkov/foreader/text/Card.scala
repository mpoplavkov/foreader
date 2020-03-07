package ru.poplavkov.foreader.text

import ru.poplavkov.foreader.Globals.WordStr

/**
  * Collection of lexical item groups with the same `lemmas`
  *
  * @author mpoplavkov
  */
case class Card(lemmas: Seq[WordStr], groups: Set[LexicalItemGroup])
