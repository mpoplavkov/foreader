package ru.poplavkov.foreader.dictionary

import cats.Id
import org.scalacheck.Gen
import ru.poplavkov.foreader.Generators._
import ru.poplavkov.foreader.Globals.WordStr
import ru.poplavkov.foreader.SpecBase

/**
  * @author mpoplavkov
  */
class MapMweSetImplSpec extends SpecBase {

  "MapMweSetImpl" should {
    "return list of words associated with key" in {
      val key = generate[WordStr]
      val value = generate[WordStr]
      val set = Set(Seq(value))
      val map = Map(key -> set)
      val mweSet = new MapMweSetImpl[Id](map)

      mweSet.getMwesStartingWith(key) shouldBe set
    }

    "return an empty set for absent key" in {
      val key = generate[WordStr]
      val anotherKey = generateSuchThat[WordStr](_ != key)
      val value = generate[WordStr]
      val map = Map(key -> Set(Seq(value)))
      val mweSet = new MapMweSetImpl[Id](map)

      mweSet.getMwesStartingWith(anotherKey) shouldBe Set.empty
    }

    "return values in order" in {
      val key = generate[WordStr]
      val i = generate[Int](Gen.chooseNum(2, 5))
      val values = Seq.fill(i)(generate[WordStr])
      val set = Set(values)
      val map = Map(key -> set)
      val mweSet = new MapMweSetImpl[Id](map)

      mweSet.getMwesStartingWith(key) shouldBe set
    }

    "return a few lists of words associated with key" in {
      val key = generate[WordStr]
      val i = generate[Int](Gen.chooseNum(2, 5))
      val set = Seq.fill(i)(Seq(generate[WordStr])).toSet
      val map = Map(key -> set)
      val mweSet = new MapMweSetImpl[Id](map)

      mweSet.getMwesStartingWith(key) shouldBe set
    }
  }
}
