import sbt._

/**
  * @author mpoplavkov
  */
object Dependencies {

  private val SoftwaremillTaggingVersion = "2.2.1"
  private val CatsEffectVersion = "2.1.1"
  private val CirceVersion = "0.13.0"
  private val EnumeratumVersion = "1.5.15"
  private val StanfordCoreNlpVersion = "3.9.2"
  private val LogbackVersion = "1.2.3"

  // test
  private val ScalatestVersion = "3.1.0"
  private val ScalacheckVersion = "1.14.3"
  private val Specs2Version = "4.8.3"
  private val ScalacheckShapelessVersion = "1.2.3"

  val test = Seq(
    "org.scalactic" %% "scalactic" % ScalatestVersion,
    "org.scalatest" %% "scalatest" % ScalatestVersion % Test,
    "org.scalacheck" %% "scalacheck" % ScalacheckVersion % Test,
    "org.specs2" %% "specs2-mock" % Specs2Version % Test,
    "com.github.alexarchambault" %% "scalacheck-shapeless_1.14" % ScalacheckShapelessVersion % Test
  )

  val stanford = Seq(
    "edu.stanford.nlp" % "stanford-corenlp" % StanfordCoreNlpVersion,
    "edu.stanford.nlp" % "stanford-corenlp" % StanfordCoreNlpVersion classifier "models-english"
  )

  val logging = Seq(
    "ch.qos.logback" % "logback-classic" % LogbackVersion
  )

  val enumeratum = Seq(
    "com.beachape" %% "enumeratum" % EnumeratumVersion
  )

  val json = Seq(
    "io.circe" %% "circe-core" % CirceVersion,
    "io.circe" %% "circe-parser" % CirceVersion
  )

  val cats = Seq(
    "org.typelevel" %% "cats-effect" % CatsEffectVersion
  )

  val tagging = Seq(
    "com.softwaremill.common" %% "tagging" % SoftwaremillTaggingVersion
  )

}
