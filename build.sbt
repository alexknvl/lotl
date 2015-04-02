assemblyJarName in assembly := "lotl.jar"

packSettings

packMain := Map("lotl" -> "lotl.Main")

proguardSettings

ProguardKeys.proguardVersion in Proguard := "5.0"

ProguardKeys.options in Proguard ++= Seq("-dontnote", "-dontwarn", "-ignorewarnings")

ProguardKeys.options in Proguard += ProguardOptions.keepMain("lotl.Main")

scalaVersion := "2.11.6"

name := "lotl"

organization := "com.alexknvl"

version := "0.1"

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots"))

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.1.1")