name := "regions-playground"

organization := "gov.wicourts"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.11.4"

resolvers += "Sonatype Snapshots Repository" at "http://oss.sonatype.org/content/repositories/snapshots"

resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"

{
  libraryDependencies ++= Seq(
    "org.scalaz" %% "scalaz-core" % "7.1.2-local-SNAPSHOT" % "compile",
    "org.scalaz" %% "scalaz-concurrent" % "7.1.2-local-SNAPSHOT" % "compile",
    "org.scalaz" %% "scalaz-effect" % "7.1.2-local-SNAPSHOT" % "compile"
  )
}

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2-core" % "3.0" % "test",
  "org.specs2" %% "specs2-matcher-extra" % "3.0" % "test"
)

scalacOptions ++= Seq("-deprecation","-feature","-Xfatal-warnings")

//scalacOptions in Test ++= Seq("-Yrangepos")

