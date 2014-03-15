organization := "org.qslib"

name := "fidl"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.10.3"

scalacOptions <++= scalaVersion map { v =>
  if (v.startsWith("2.10"))
    Seq("-unchecked", "-deprecation", "-feature", "-language:implicitConversions")
  else
    Seq("-unchecked", "-deprecation")
}

libraryDependencies ++= Seq(
  "com.chuusai" % "shapeless_2.10.2" % "2.0.0-M1",
  "com.github.nscala-time" %% "nscala-time" % "0.6.0",
  "org.scalatest" %% "scalatest" % "2.0" % "test",
  "org.scalacheck" %% "scalacheck" % "1.11.0" % "test",
  "junit" % "junit" % "4.11" % "test"
)

resolvers ++= Seq(
  "Sonatype OSS Releases"  at "http://oss.sonatype.org/content/repositories/releases/",
  "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"
)
