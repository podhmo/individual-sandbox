import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.12.4",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "hello",
    libraryDependencies += scalaTest % Test
  )
  .aggregate(hello)

lazy val greeting = (project in file("greeting"))

lazy val hello = (project in file("hello"))
  .dependsOn(greeting)
