package ru.poplavkov.foreader

import ru.poplavkov.foreader.Generators._

/**
  * @author mpoplavkov
  */
class CollectionUtilSpec extends SpecBase {

  private val key1 = generate[String]
  private val key2 = generateSuchThat[String](_ != key1)
  private val values1 = generate[Seq[String]]
  private val values2 = generate[Seq[String]]
  private val values3 = generate[Seq[String]]

  "CollectionUtil.mergeMaps" should {
    "merge tow maps without intersected keys" in {
      val map1 = Map(key1 -> values1)
      val map2 = Map(key2 -> values2)
      val expected = Map(key1 -> values1, key2 -> values2)
      CollectionUtil.mergeMaps(map1, map2)(_ ++ _) shouldBe expected
    }

    "merge tow maps with intersected keys" in {
      val map1 = Map(key1 -> values1)
      val map2 = Map(key1 -> values2)
      val expected = Map(key1 -> (values1 ++ values2))
      CollectionUtil.mergeMaps(map1, map2)(_ ++ _) shouldBe expected
    }

    "merge tow maps with both intersected keys and not" in {
      val map1 = Map(key1 -> values1, key2 -> values3)
      val map2 = Map(key1 -> values2)
      val expected = Map(key1 -> (values1 ++ values2), key2 -> values3)
      CollectionUtil.mergeMaps(map1, map2)(_ ++ _) shouldBe expected
    }

    "concat values as a reduce function" in {
      val map1 = Map(key1 -> values1)
      val map2 = Map(key1 -> values2)
      val expected = Map(key1 -> (values1 ++ values2))
      CollectionUtil.mergeMaps(map1, map2)(_ ++ _) shouldBe expected
    }

    "apply `+` as a reduce function" in {
      val valueInt1 = generate[Int]
      val valueInt2 = generate[Int]
      val map1 = Map(key1 -> valueInt1)
      val map2 = Map(key1 -> valueInt2)
      val expected = Map(key1 -> (valueInt1 + valueInt2))
      CollectionUtil.mergeMaps(map1, map2)(_ + _) shouldBe expected
    }

  }

}
