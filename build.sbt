import sbtrelease.ReleaseStateTransformations._

name := "TwoFactorAuth"

version := "0.1"

scalaVersion := "2.12.7"

version in ThisBuild := "$releaseVersion"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest"     % "3.0.5" % Test
)

useGpg := true

organization := "com.bunyod"
homepage := Some(url("https://github.com/Bunyod/TwoFactorAuth"))
scmInfo := Some(ScmInfo(url("https://github.com/Bunyod/TwoFactorAuth"),
                            "git@github.com:Bunyod/TwoFactorAuth.git")
)
developers := List(Developer("Bunyod",
  "Bunyod",
  "bunyodreal@gmail.com",
  url("https://github.com/bunyod")))
licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0"))
publishMavenStyle := true

pomIncludeRepository := { _ => false }
// Add sonatype repository settings
publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
  else Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

publishMavenStyle := true

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
