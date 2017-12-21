name := "aoc"
organization := "org.cinple"
version := "0.1-SNAPSHOT"
scalaVersion := "2.11.8"
scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

val log4jVersion           = "2.2"
val scalacheckVersion      = "1.11.4"
val scalatestVersion       = "2.2.4"
val slf4jVersion           = "1.7.12"

resolvers ++= Seq(
  "Artifactory" at "https://repo.artifacts.weather.com/analytics-virtual",
  Resolver.mavenLocal
)

libraryDependencies ++= Seq(
  "org.apache.logging.log4j"     % "log4j-api"             % log4jVersion,
  "org.slf4j"                    % "slf4j-api"             % slf4jVersion,
  "org.scalacheck"              %% "scalacheck"            % scalacheckVersion       % "test",
  "org.scalatest"               %% "scalatest"             % scalatestVersion        % "test",
  "org.slf4j"                    % "slf4j-simple"          % slf4jVersion            % "test"
)

parallelExecution in Test := false
