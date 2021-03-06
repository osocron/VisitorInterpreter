enablePlugins(ScalaJSPlugin)

name := "DesignPatterns"

version := "1.0"

scalaVersion := "2.12.1"

libraryDependencies ++= Seq(
  "com.github.thangiee" %% "freasy-monad" % "0.5.0",
  "org.typelevel" %% "cats" % "0.8.1")

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)