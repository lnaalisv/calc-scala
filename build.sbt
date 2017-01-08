lazy val root = (project in file(".")).
  settings(
    name := "calc",
    version := "0.1",
    scalaVersion := "2.12.1"
  )

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.8"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"