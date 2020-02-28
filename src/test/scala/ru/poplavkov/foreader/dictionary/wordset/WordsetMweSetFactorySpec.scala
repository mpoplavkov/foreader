package ru.poplavkov.foreader.dictionary.wordset

import cats.Id
import com.softwaremill.tagging._
import ru.poplavkov.foreader.Generators._
import ru.poplavkov.foreader.Globals.{WordStr, WordStrTag}
import ru.poplavkov.foreader.SpecBase
import ru.poplavkov.foreader.SpecBase._
import ru.poplavkov.foreader.dictionary.MweSet

/**
  * @author mpoplavkov
  */
class WordsetMweSetFactorySpec extends SpecBase {

  val factory = new WordsetMweSetFactory[Id](getResourcePath("/wordset"), fileNames = Set("a.json", "l.json"))
  val mweSet: MweSet[Id] = factory.createMweSet()

  "WordsetMweSet" should {

    "extract single MWE from dictionary" in {
      val actual = mweSet.getMwesStartingWith("all".taggedWith[WordStrTag])
      actual shouldBe Set(Seq("right"))
    }

    "extract long MWE from dictionary" in {
      val actual: Set[Seq[WordStr]] = mweSet.getMwesStartingWith("a".taggedWith[WordStrTag])
      actual shouldBe Set(Seq("day", "late", "and", "a", "dollar", "short"))
    }

    "extract a few MWEs from dictionary" in {
      val actual = mweSet.getMwesStartingWith("long".taggedWith[WordStrTag])
      actual shouldBe Set(Seq("ago"), Seq("since"))
    }

    "extract MWEs from different files" in {
      val actual1 = mweSet.getMwesStartingWith("all".taggedWith[WordStrTag])
      val actual2 = mweSet.getMwesStartingWith("let".taggedWith[WordStrTag])
      actual1 shouldBe Set(Seq("right"))
      actual2 shouldBe Set(Seq("alone"))
    }

    "not extract MWE for single key in dictionary" in {
      val actual = mweSet.getMwesStartingWith("about".taggedWith[WordStrTag])
      actual shouldBe Set.empty
    }

    "not extract MWE for absent key in dictionary" in {
      val actual = mweSet.getMwesStartingWith(generate[WordStr])
      actual shouldBe Set.empty
    }

  }

}
