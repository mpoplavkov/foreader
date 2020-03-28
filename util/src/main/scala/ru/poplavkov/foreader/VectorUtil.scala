package ru.poplavkov.foreader

import ru.poplavkov.foreader.vector.MathVector
import smile.clustering

import scala.language.implicitConversions

/**
  * @author mpoplavkov
  */
object VectorUtil {

  def euclideanDistance(vectorA: MathVector, vectorB: MathVector): Float =
    (vectorA - vectorB).length

  def avgVector(dimension: Int, vectors: Seq[MathVector]): MathVector =
    vectors.foldLeft(MathVector.zero(dimension))(_ + _) / vectors.size

  def equalsWithAllowedDistanceBetween(vectorA: MathVector,
                                       vectorB: MathVector,
                                       allowedDistance: Float = 0.01f): Boolean =
    euclideanDistance(vectorA, vectorB) < allowedDistance

  /**
    * K-Means clustering
    * Uses squared Euclidean distance
    *
    * @param k       desired number of clusters
    * @param vectors vectors to cluster
    * @return sequence of cluster centroids
    */
  def kmeans(k: Int, vectors: Seq[MathVector]): Seq[MathVector] = {
    val means = clustering.kmeans(vectors, k)
    means.centroids.map(doublesArray2MathVector)
  }

  private def doublesArray2MathVector(array: Array[Double]): MathVector =
    MathVector(coordinates = array.map(_.toFloat))

  private def mathVector2DoublesArray(vector: MathVector): Array[Double] =
    vector.coordinates.map(_.toDouble).toArray

  private implicit def mathVectors2DoublesArrays(vectors: Seq[MathVector]): Array[Array[Double]] =
    vectors.toArray.map(mathVector2DoublesArray)

}
