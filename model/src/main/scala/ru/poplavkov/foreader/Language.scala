package ru.poplavkov.foreader

import enumeratum._

import scala.collection.immutable

/**
  * @author mpoplavkov
  */
sealed trait Language extends EnumEntry

object Language extends Enum[Language] {

  override def values: immutable.IndexedSeq[Language] = findValues

  case object English extends Language

}
