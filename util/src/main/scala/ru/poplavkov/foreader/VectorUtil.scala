package ru.poplavkov.foreader

import ru.poplavkov.foreader.vector.MathVector

/**
  * @author mpoplavkov
  */
object VectorUtil {

  def euclideanDistance(vectorA: MathVector, vectorB: MathVector): Float =
    (vectorA - vectorB).length

  def avgVector(dimension: Int, vectors: Seq[MathVector]): MathVector =
    vectors.foldLeft(MathVector.zero(dimension))(_ + _) / vectors.size

}
