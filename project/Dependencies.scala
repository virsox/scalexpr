import sbt._
import Keys._

object Dependencies {


  val logback        = "ch.qos.logback" % "logback-classic" % "1.1.3"
  val scalaCompiler  = "org.scala-lang" % "scala-compiler" % "2.11.7"
  val scalaReflect   = "org.scala-lang" % "scala-reflect"  % "2.11.7"
  val fastParse      = "com.lihaoyi" %% "fastparse" % "0.3.7"

  // ------------------- test
  val junit     = "junit" % "junit" % "4.11" % Test
  val mockito   = "org.mockito" % "mockito-core" % "1.9.5" % Test
  val scalaTest = "org.scalatest" %% "scalatest" % "2.2.6" % Test

  val baseDeps  = Seq(logback, fastParse, scalaCompiler, scalaReflect)
  val testDeps  = Seq(junit, mockito, scalaTest)



}