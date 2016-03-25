import bintray.Keys._

lazy val commonSettings = Seq(
  version in ThisBuild := "0.5.0",
  organization in ThisBuild := "com.github.davidpeklak"
)

lazy val root = (project in file(".")).
  settings(commonSettings ++ bintrayPublishSettings: _*).
  settings(
    sbtPlugin := true,
    name := "smt",
    description := "scala database migration tool",
    licenses += ("MIT", url("http://opensource.org/licenses/MIT")),
    publishMavenStyle := false,
    repository in bintray := "sbt-plugins",
    bintrayOrganization in bintray := None
  )


libraryDependencies ++= Seq(
  "com.h2database" % "h2" % "1.3.172",
  "org.scalaz" %% "scalaz-core" % "7.0.5",
  "org.scalaz" %% "scalaz-concurrent" % "7.0.5",
  "javax.mail" % "mail" % "1.4.7",
  "org.scalacheck" %% "scalacheck" % "1.10.1" % "test",
  "org.scalatest" %% "scalatest" % "1.9.2" % "test"
)
