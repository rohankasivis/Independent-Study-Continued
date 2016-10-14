name := "distributed-aggregation"

version := "0.99"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.4.8",
  "com.typesafe.akka" %% "akka-testkit" % "2.4.8",
  "org.scalatest" % "scalatest_2.11" % "2.2.5"
)
