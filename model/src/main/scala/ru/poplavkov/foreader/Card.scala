package ru.poplavkov.foreader

import ru.poplavkov.foreader.Globals.WordStr
import ru.poplavkov.foreader.text.LexicalItemGroup

/**
  * Collection of lexical item groups with the same `lemmas`
  *
  * @author mpoplavkov
  */
case class Card(lemmas: Seq[WordStr], groups: Set[LexicalItemGroup])
