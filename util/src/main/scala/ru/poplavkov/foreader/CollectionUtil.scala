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
    val commonKeys = map1.keySet.intersect(map2.keySet)
    val updatedCommon = commonKeys.map { k =>
      k -> reduce(map1(k), map2(k))
    }.toMap

    map1 ++ map2 ++ updatedCommon
  }

  def mergeMapsWithUniqueKeys[K, V](map1: Map[K, V], map2: Map[K, V]): Map[K, V] =
    mergeMaps(map1, map2)((_, _) => throw new IllegalArgumentException("Not unique keys"))

  /**
    * Looks for the most frequent occurrence among given sequence
    *
    * @return the most frequent element with its number
    */
  def mostFrequentElement[A](seq: Seq[A]): (A, Int) = {
    require(seq.nonEmpty)
    seq.groupBy(identity).mapValues(_.size).maxBy(_._2)
  }

}
