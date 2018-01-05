// see: https://github.com/sbt/sbt-assembly

name := """hello"""
version := "1.0"

lazy val commonSettings = Seq(
  version := "0.1-SNAPSHOT",
  organization := "com.example",
  scalaVersion := "2.10.6",
  test in assembly := {}
)

lazy val app = project.in(file("src"))
  .settings(commonSettings: _*)
  .settings(mainClass in assembly := Some("foo.bar.baz.Main"))

// assemblyOption in assembly := (assemblyOption in assembly).value.copy(includeScala = false, includeDependency = false)
