organization := "com.mattwittmann.scabasic"

name := "sca_basic"

version := "1.0.0-SNAPSHOT"

scalaVersion := "2.11.1"

val scalazVersion = "7.0.6"

javacOptions ++= Seq("-source", "1.8", "-target", "1.8")

libraryDependencies ++= Seq("org.scalaz" %% "scalaz-core" % scalazVersion,
                            "org.scalaz" %% "scalaz-effect" % scalazVersion,
                            "org.scalatest" %% "scalatest" % "2.2.0" % "test")

scalacOptions ++= Seq("-feature", "-deprecation")
