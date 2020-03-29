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
    * Will find `k` cluster centroids iff distinct vectors size >= `k`
    *
    * @param k       desired number of clusters
    * @param vectors vectors to cluster
    * @return sequence of cluster centroids or `None` if it couldn't find `k` clusters
    */
  def kmeans(k: Int, vectors: Seq[MathVector]): Option[Seq[MathVector]] =
    if (vectors.isEmpty) {
      None
    } else if (k == 1) {
      Some(Seq(avgVector(vectors.head.dimension, vectors)))
    } else {
      val means = clustering.kmeans(vectors, k)
      val labels = means.y
      if (labels.toSet.size < k) {
        // Couldn't find `k` clusters
        None
      } else {
        Some(means.centroids.map(doublesArray2MathVector))
      }
    }

  private def doublesArray2MathVector(array: Array[Double]): MathVector =
    MathVector(coordinates = array.map(_.toFloat))

  private def mathVector2DoublesArray(vector: MathVector): Array[Double] =
    vector.coordinates.map(_.toDouble).toArray

  private implicit def mathVectors2DoublesArrays(vectors: Seq[MathVector]): Array[Array[Double]] =
    vectors.toArray.map(mathVector2DoublesArray)

}
