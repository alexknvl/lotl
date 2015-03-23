scalaVersion := "2.11.5"

name := "lotl"

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots"))

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.1.1",
  "org.spire-math" %% "spire" % "0.9.0",
  "com.chuusai" %% "shapeless" % "2.1.0",
  "org.scalatest" %% "scalatest" % "2.1.3" % "test",
  "org.scalacheck" %% "scalacheck" % "1.12.2" % "test",
  "org.typelevel" %% "discipline" % "0.2.1" % "test")