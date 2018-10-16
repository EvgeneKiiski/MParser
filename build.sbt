
resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"

logBuffered in Test := false

//libraryDependencies += "junit" % "junit" % "4.12" % Test
//libraryDependencies += "org.typelevel" %% "discipline" % "0.10.0" % Test
//libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.5"
//libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % Test

lazy val commonSettings = Seq(
  organization := "ru.twistedlogic",
  version := "0.0.1-SNAPSHOT",
  scalaVersion := "2.12.6"
)

lazy val root = (project in file("."))
  .settings(
    commonSettings,
    name := "MParser",
    libraryDependencies ++= Seq(
      "junit" % "junit" % "4.12" % Test,
      "org.typelevel" %% "discipline" % "0.10.0" % Test,
      "org.scalactic" %% "scalactic" % "3.0.5",
      "org.scalatest" %% "scalatest" % "3.0.5" % Test
    )
  )


lazy val examples = (project in file("examples"))
  .dependsOn(root)
  .settings(
    commonSettings,
    name := "examples"
  )





