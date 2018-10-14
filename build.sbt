name := "MParser"

version := "0.1"

scalaVersion := "2.12.6"

resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"

logBuffered in Test := false

libraryDependencies += "junit" % "junit" % "4.12" % Test
libraryDependencies += "org.typelevel" %% "discipline" % "0.10.0" % Test
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.5"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % Test


