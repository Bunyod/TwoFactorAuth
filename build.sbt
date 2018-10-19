import sbtrelease.ReleaseStateTransformations._

name := "TwoFactorAuth"

version := "0.1"

scalaVersion := "2.12.7"

version in ThisBuild := "$releaseVersion"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest"     % "3.0.5" % Test
)

releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,
  inquireVersions,
  runClean,
  runTest,
  setReleaseVersion,
  publishArtifacts,
  commitReleaseVersion,
  tagRelease,
  setNextVersion,
  commitNextVersion,
  pushChanges
)
