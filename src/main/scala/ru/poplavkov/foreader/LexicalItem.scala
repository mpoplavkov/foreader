package ru.poplavkov.foreader

/**
  * Single word, a part of a word, or a chain of words
  * Lexical items can be generally understood to convey a single meaning
  *
  * @author mpoplavkov
  */
trait LexicalItem {

  /**
    * Canonical form of the item used in dictionaries
    */
  def canonicalForm: String

  /**
    * Original form of the item in the text
    */
  def originalForm: String

  /**
    * Starting position of the item in the text
    */
  def position: Int

}
