val scala3Version = "3.6.3"

lazy val root = project
  .in(file("."))
  .settings(
    name := "Scala 3 Project Template",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq("org.scalameta" %% "munit" % "1.0.0" % Test,
    "org.scala-lang.modules" %% "scala-parallel-collections"% "1.0.3",
     "org.typelevel" %% "cats-core" % "2.13.0")
  )
