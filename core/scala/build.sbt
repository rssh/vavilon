
lazy val commonSettings = Seq(
  organization := "com.github.rssh",
  scalaVersion := "2.12.1",
  scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature"),
  libraryDependencies ++= Seq(
    scalaVersion("org.scala-lang" % "scala-reflect" % _).value,
    "com.chuusai" %% "shapeless" % "2.3.2",
    "org.typelevel" %% "cats" % "0.9.0",
    "com.typesafe.scala-logging" %% "scala-logging" % "3.5.0",
    "ch.qos.logback" % "logback-classic" % "1.1.7",
    "org.scalatest" %% "scalatest" % "3.0.1" % Test
  )
)

lazy val macro = (project in file("macro"))
  .settings(commonSettings: _*)
  .settings(
    name := "macro"
  )

lazy val termware = (project in file("termware"))
  .settings(commonSettings: _*)
  .settings(
    name := "termware"
  ).dependsOn(macro)

lazy val examples = (project in file("examples"))
  .settings(commonSettings: _*)
  .settings(
    name := "examples"
  ).dependsOn(macro)


lazy val root = Project(id = "vavilon", base = file("."))
  .settings(commonSettings: _*)
  .settings(
    name := "vavilon"
  )
  .aggregate(
    macro,
    termware,
    examples
  )


  
