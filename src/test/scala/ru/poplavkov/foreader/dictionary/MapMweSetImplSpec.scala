package ru.poplavkov.foreader.dictionary

import cats.Id
import org.scalacheck.Gen
import ru.poplavkov.foreader.Generators._
import ru.poplavkov.foreader.Globals.Word
import ru.poplavkov.foreader.SpecBase

/**
  * @author mpoplavkov
  */
class MapMweSetImplSpec extends SpecBase {

  "MapMweSetImpl" should {
    "return list of words associated with key" in {
      val key = generate[Word]
      val value = generate[Word]
      val set = Set(List(value))
      val map = Map(key -> set)
      val mweSet = new MapMweSetImpl[Id](map)

      mweSet.getMwesStartingWith(key) shouldBe set
    }

    "return an empty set for absent key" in {
      val key = generate[Word]
      val anotherKey = generateSuchThat[Word](_ != key)
      val value = generate[Word]
      val map = Map(key -> Set(List(value)))
      val mweSet = new MapMweSetImpl[Id](map)

      mweSet.getMwesStartingWith(anotherKey) shouldBe Set.empty
    }

    "return values in order" in {
      val key = generate[Word]
      val i = generate[Int](Gen.chooseNum(2, 5))
      val values = List.fill(i)(generate[Word])
      val set = Set(values)
      val map = Map(key -> set)
      val mweSet = new MapMweSetImpl[Id](map)

      mweSet.getMwesStartingWith(key) shouldBe set
    }

    "return a few lists of words associated with key" in {
      val key = generate[Word]
      val i = generate[Int](Gen.chooseNum(2, 5))
      val set = List.fill(i)(List(generate[Word])).toSet
      val map = Map(key -> set)
      val mweSet = new MapMweSetImpl[Id](map)

      mweSet.getMwesStartingWith(key) shouldBe set
    }
  }
}
