val scala3Version = "3.1.0"
import ai.kien.python.Python

lazy val python = Python(
  "/opt/homebrew/Caskroom/miniconda/base/envs/scalapy/bin/python"
)

lazy val javaOpts = python.scalapyProperties.get.map { case (k, v) =>
  s"""-D$k=$v"""
}.toSeq

lazy val root = project
  .in(file("."))
  .settings(
    name := "Advent 2021",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    javaOptions := javaOpts,
    libraryDependencies ++= Seq(
      "com.novocode" % "junit-interface" % "0.11" % "test",
      "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4",
      "me.shadaj" %% "scalapy-core" % "0.5.1+1-a19bda77",
      "com.github.j-mie6" %% "parsley" % "3.2.1"
    )
  )
