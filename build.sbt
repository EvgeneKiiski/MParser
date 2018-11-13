
resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"

logBuffered in Test := false

lazy val commonSettings = Seq(
  organization := "org.mparser",
  version := "0.0.3",
  scalaVersion := "2.12.7"
)

lazy val root = (project in file("."))
  .settings(
    commonSettings,
    name := "mparser",
    libraryDependencies ++= Seq(
      "junit" % "junit" % "4.12" % Test,
      "org.typelevel" %% "discipline" % "0.10.0" % Test,
      "org.scalactic" %% "scalactic" % "3.0.5"  % Test,
      "org.scalatest" %% "scalatest" % "3.0.5" % Test
    ),
    publishTo := Some(Resolver.file("file",  new File( "/Users/evg/work/mparser.org/repository" )) )
  )


lazy val examples = (project in file("examples"))
  .dependsOn(root)
  .settings(
    commonSettings,
    name := "examples"
  )

lazy val benchmarks = (project in file("benchmarks"))
  .enablePlugins(JmhPlugin)
  .dependsOn(root, examples)
  .settings(
    commonSettings,
    name := "benchmarks",
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core" % "0.10.0",
      "io.circe" %% "circe-generic" % "0.10.0",
      "io.circe" %% "circe-parser" % "0.10.0",
      "junit" % "junit" % "4.12" % Test,
      "org.typelevel" %% "discipline" % "0.10.0" % Test,
      "org.scalactic" %% "scalactic" % "3.0.5",
      "org.scalatest" %% "scalatest" % "3.0.5" % Test
    )
  )







