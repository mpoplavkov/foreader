package ru.poplavkov.foreader

/**
  * @author mpoplavkov
  */
object CollectionUtil {

  /**
    * Merges two maps. For values associated with the same key in both maps
    * function `reduce` is applied
    *
    * @return merged map
    */
  def mergeMaps[K, V](map1: Map[K, V], map2: Map[K, V])
                     (reduce: (V, V) => V): Map[K, V] = {
    val combined = map1.map { case (k, v1) =>
      val newValue = map2.get(k).map(reduce(v1, _)).getOrElse(v1)
      k -> newValue
    }
    map2 ++ combined
  }

  def mergeMapsWithUniqueKeys[K, V](map1: Map[K, V], map2: Map[K, V]): Map[K, V] =
    mergeMaps(map1, map2)((_, _) => throw new IllegalArgumentException("Not unique keys"))

  /**
    * Extracts `left` elements that are left-hand to the `index` element and
    * `right` elements that are right-hand to the `index` element and concatenates
    * them to the resulting seq. Element with the `index` position not included in
    * result
    */
  def surroundingElements[T](seq: Seq[T], index: Int, left: Int, right: Int): Seq[T] = {
    val fromLeft = (index - left) max 0
    val toRight = (index + right) min (seq.length - 1)
    val indices = (fromLeft until index) ++ (toRight until index by -1)
    indices.map(seq.apply)
  }

}
