val scala3Version = "3.1.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "Advent 2021",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,
    libraryDependencies ++= Seq("com.novocode" % "junit-interface" % "0.11" % "test",
    "com.github.j-mie6" %% "parsley" % "3.2.1")
  )
