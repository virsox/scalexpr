import Dependencies._

scalacOptions in ThisBuild ++= Seq("-deprecation", "-feature")

organization := "br.com.virsox.scalexpr"

name := "scalexpr"

version := "0.0.1-SNAPSHOT"

isSnapshot := true

scalaVersion := "2.13.2"

libraryDependencies ++= (baseDeps ++ testDeps)
