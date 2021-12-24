name := "aoc-21-8"
description := "Day 8: Seven Segment Search"
version := "0.1-SNAPSHOT"
scalaVersion := "3.1.0"

libraryDependencies ++= {
  Seq(
    "org.scalatest" %% "scalatest"   % "3.2.10" % Test,
    "org.typelevel" %% "cats-core"   % "2.7.0",
    ("org.scalaj"   %% "scalaj-http" % "2.4.2").cross(CrossVersion.for3Use2_13)
  )
}
