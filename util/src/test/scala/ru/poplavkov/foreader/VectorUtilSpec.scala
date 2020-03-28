package ru.poplavkov.foreader

import ru.poplavkov.foreader.VectorUtilSpec._
import ru.poplavkov.foreader.vector.MathVector

import scala.util.Try

/**
  * @author mpoplavkov
  */
class VectorUtilSpec extends SpecBase {

  private val matrixStr =
    """. . . . . . . . .
      |. 1 1 . . . 2 2 .
      |. 1 1 . . . 2 2 .
      |. . . . . . . . .
      |. . . . . . . . .
      |. . . . . . . . .
      |. . . . . . . . .
      |. 3 3 . . . 4 4 .
      |. 3 3 . . . 4 4 .
      |. . . . . . . . .""".stripMargin

  private val clusters = parseMatrix(matrixStr)
  private val vectors = clusters.values.flatten.toSeq

  private val cases = Seq(
    TestCase(
      clustersNum = 1,
      expectedCentroids = Seq(
        VectorUtil.avgVector(dimension = 2, clusters(1) ++ clusters(2) ++ clusters(3) ++ clusters(4))
      )
    ),
    TestCase(
      clustersNum = 2,
      expectedCentroids = Seq(
        VectorUtil.avgVector(dimension = 2, clusters(1) ++ clusters(2)),
        VectorUtil.avgVector(dimension = 2, clusters(3) ++ clusters(4))
      )
    ),
    TestCase(
      clustersNum = 4,
      expectedCentroids = Seq(
        VectorUtil.avgVector(dimension = 2, clusters(1)),
        VectorUtil.avgVector(dimension = 2, clusters(2)),
        VectorUtil.avgVector(dimension = 2, clusters(3)),
        VectorUtil.avgVector(dimension = 2, clusters(4))
      )
    )
  )

  "VectorUtil.kmeans" should {
    cases.foreach { case TestCase(clustersNum, expectedCentroids) =>
      s"find $clustersNum clusters" in {
        val centroids = VectorUtil.kmeans(clustersNum, vectors)
        val similarityMap: Map[MathVector, Seq[MathVector]] =
          expectedCentroids.map { expected =>
            expected -> centroids.filter(areSimilar(_, expected))
          }.toMap

        // every expected centroid maps to exactly one actual centroid
        similarityMap.values.foreach(_.size shouldBe 1)

        // every actual centroid is present in the map
        similarityMap.values.flatten should contain theSameElementsAs centroids
      }
    }
  }
}

object VectorUtilSpec {

  case class TestCase(clustersNum: Int, expectedCentroids: Seq[MathVector])

  /**
    * Builds 2-dimensional vectors using string matrix representation,
    * where vector is defined as a number
    *
    * @return mapping from the number to vectors, associated with it
    */
  private def parseMatrix(matrixStr: String): Map[Int, Seq[MathVector]] = {
    val matrix = matrixStr.split("\n").map(_.split(" "))
    val pairs = for {
      (row, rowInd) <- matrix.zipWithIndex
      (col, colInd) <- row.zipWithIndex
      number <- Try(col.toInt).toOption
    } yield (number, MathVector(Seq(rowInd, colInd)))
    pairs.groupBy(_._1).mapValues(_.map(_._2))
  }

  private def areSimilar(a: MathVector, b: MathVector): Boolean =
    VectorUtil.equalsWithAllowedDistanceBetween(a, b, allowedDistance = 1)

}
