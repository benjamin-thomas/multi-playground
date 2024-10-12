val scala3Version = "3.5.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "radix",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies += "org.scalameta" %% "munit" % "1.0.0" % Test
  )

// sbt:radix> ~clear;compile
// sbt:radix> ~clear;run
commands += Command.command("clear") { state =>
  print("\u001bc")
  state
}
