package ru.poplavkov.foreader

import ru.poplavkov.foreader.Globals.WordStr

package object dictionary {

  type DictionaryMap = Map[Seq[WordStr], DictionaryEntry]
  type MweMap = Map[WordStr, Set[Seq[WordStr]]]

  def dictionaryMapToMweMap(dictionaryMap: DictionaryMap): MweMap =
    dictionaryMap.keys
      .map(_.toList)
      .collect { case start :: rest if rest.nonEmpty => start -> rest }
      .groupBy(_._1)
      .mapValues(_.map(_._2).toSet)

}
