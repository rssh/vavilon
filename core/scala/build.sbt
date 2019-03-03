
lazy val commonSettings = Seq(
  organization := "com.github.rssh",
  version := "0.0.1",
  scalaVersion := "2.12.8",
  scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Ypartial-unification"),
  libraryDependencies ++= Seq(
    scalaVersion("org.scala-lang" % "scala-reflect" % _).value,
    "com.chuusai" %% "shapeless" % "2.3.2",
    "org.typelevel" %% "cats-core" % "1.1.0",
    "org.typelevel" %% "cats-effect" % "1.0.0-RC",
    "com.typesafe.scala-logging" %% "scala-logging" % "3.5.0",
    "ch.qos.logback" % "logback-classic" % "1.1.7",
    "com.lihaoyi" %% "fastparse" % "2.1.0",
    "org.scalatest" %% "scalatest" % "3.0.1" % Test
  )
)


lazy val macroses = (project in file("macroses"))
  .settings(commonSettings: _*)
  .settings(
    name := "termware-macroses"
  )

lazy val termware = (project in file("termware2"))
  .settings(commonSettings: _*)
  .settings(
    name := "termware"
  ).dependsOn(macroses)

lazy val examples = (project in file("examples"))
  .settings(commonSettings: _*)
  .settings(
    name := "examples"
  ).dependsOn(macroses)


lazy val root = Project(id = "vavilon", base = file("."))
  .settings(commonSettings: _*)
  .settings(
    name := "vavilon"
  )
  .aggregate(
    macroses,
    termware,
    examples
  )

