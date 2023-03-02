ThisBuild / version := "1.0.0"

ThisBuild / scalaVersion := "3.2.2"

lazy val root = (project in file("."))
  .settings(
    name := "MonkeyLang-Scala"
  )

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.2.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.15" % Test
