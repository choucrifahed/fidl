organization := "org.qslib"

name := "fidl"

version := "0.1.0"

scalaVersion := "2.10.3"

scalacOptions <++= scalaVersion map { v =>
  if (v.startsWith("2.10"))
    Seq("-unchecked", "-deprecation", "-feature", "-language:implicitConversions")
  else
    Seq("-unchecked", "-deprecation")
}

libraryDependencies ++= Seq(
  "com.github.nscala-time" %% "nscala-time" % "0.6.0",
  "org.scalatest" %% "scalatest" % "1.9.2" % "test",
  "org.scalacheck" %% "scalacheck" % "1.10.1" % "test",
  "junit" % "junit" % "4.11" % "test"
)

resolvers ++= Seq(
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"
)
