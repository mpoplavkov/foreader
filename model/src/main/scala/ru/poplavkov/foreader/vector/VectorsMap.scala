package ru.poplavkov.foreader.vector

import ru.poplavkov.foreader.Globals.WordStr

/**
  * @author mpoplavkov
  */
class VectorsMap(val dimension: Int, private val map: Map[WordStr, MathVector]) {

  require(map.values.forall(_.dimension == dimension))

  def getVector(word: WordStr): Option[MathVector] = map.get(word)

}

object VectorsMap {

  val Empty: VectorsMap = new VectorsMap(0, Map.empty)

}
