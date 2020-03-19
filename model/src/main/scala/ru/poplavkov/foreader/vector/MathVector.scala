package ru.poplavkov.foreader.vector

/**
  * @author mpoplavkov
  */
case class MathVector(coordinates: Seq[Float]) {

  val dimension: Int = coordinates.size

  def +(other: MathVector): MathVector = mathOperation(other)(_ + _)

  def -(other: MathVector): MathVector = mathOperation(other)(_ - _)

  def /(i: Int): MathVector = copy(coordinates = coordinates.map(_ / i))

  def length: Float = Math.sqrt(coordinates.map(Math.pow(_, 2)).sum).toFloat

  private def mathOperation(other: MathVector)(op: (Float, Float) => Float): MathVector = {
    require(dimension == other.dimension) // TODO: get rid of exception
    val coords = coordinates.zip(other.coordinates).map { case (a, b) => op(a, b) }
    MathVector(coords)
  }

}

object MathVector {

  def zero(dim: Int): MathVector = MathVector(Seq.fill(dim)(0))

}
