import bintray.Keys._

scalaVersion := "2.12.2"

lazy val commonSettings = Seq(
  version in ThisBuild := "0.6.1",
  organization in ThisBuild := "com.github.davidpeklak"
)

lazy val root = (project in file(".")).
  settings(commonSettings ++ bintrayPublishSettings: _*).
  settings(
    name := "smt-lib",
    description := "scala database migration library",
    licenses += ("MIT", url("http://opensource.org/licenses/MIT")),
    publishMavenStyle := true,
    repository in bintray := "maven",
    bintrayOrganization in bintray := None
  )


libraryDependencies ++= Seq(
  "com.h2database" % "h2" % "1.3.172" % "test",
  "org.scalaz" %% "scalaz-core" % "7.2.14",
  "org.scalacheck" %% "scalacheck" % "1.13.4" % "test",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test"
)
