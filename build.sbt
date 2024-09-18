val scala3Version = "2.13.11"

lazy val root = project
  .in(file("."))
  .settings(
    name := "ScalaAssignment",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,

    // Add this line to include the root directory
    unmanagedSourceDirectories in Compile += baseDirectory.value,

    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test
  )
