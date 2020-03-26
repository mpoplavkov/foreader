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

}
