lazy val commonSettings = Seq(
  organization := "com.example",
  version := "0.1.0",
  scalaVersion := "2.11.7"
)

lazy val root = (project in file(".")).
  settings(commonSettings:_*).
  settings(
    name := "sudukoSolver",
    scalaVersion := "2.11.7",
    libraryDependencies += "org.specs2" %% "specs2-core" % "3.0" % "test",
	resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases",
	scalacOptions in Test ++= Seq("-Yrangepos")
  )
