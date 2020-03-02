name := "foreader"

version := "0.1"

scalaVersion := "2.12.7"

val SoftwaremillTaggingVersion = "2.2.1"
val CatsEffectVersion = "2.1.1"
val CirceVersion = "0.13.0"
val EnumeratumVersion = "1.5.15"
val StanfordCoreNlpVersion = "3.9.2"
val LogbackVersion = "1.2.3"

val ScalatestVersion = "3.1.0"
val ScalacheckVersion = "1.14.3"
val Specs2Version = "4.8.3"
val ScalacheckShapelessVersion = "1.2.3"

libraryDependencies ++= Seq(
  "com.softwaremill.common" %% "tagging" % SoftwaremillTaggingVersion,
  "org.typelevel" %% "cats-effect" % CatsEffectVersion,
  "io.circe" %% "circe-core" % CirceVersion,
  "io.circe" %% "circe-parser" % CirceVersion,
  "com.beachape" %% "enumeratum" % EnumeratumVersion,
  "edu.stanford.nlp" % "stanford-corenlp" % StanfordCoreNlpVersion,
  "edu.stanford.nlp" % "stanford-corenlp" % StanfordCoreNlpVersion classifier "models-english",
  "ch.qos.logback" % "logback-classic" % LogbackVersion,
  "org.scalactic" %% "scalactic" % ScalatestVersion,

  "org.scalatest" %% "scalatest" % ScalatestVersion % Test,
  "org.scalacheck" %% "scalacheck" % ScalacheckVersion % Test,
  "org.specs2" %% "specs2-mock" % Specs2Version % Test,
  "com.github.alexarchambault" %% "scalacheck-shapeless_1.14" % ScalacheckShapelessVersion % Test
)

resolvers += Resolver.sonatypeRepo("releases")
