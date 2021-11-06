import Dependencies._
import Resolvers._
import BuildSettings._
import sbtassembly.AssemblyKeys.assemblyMergeStrategy
import sbtassembly.MergeStrategy

lazy val buildSettings = Seq(
    organization := Organization,
    name := Name,
    version := Version,
    scalaVersion := VersionScala
)

javacOptions ++= Seq("-encoding", "UTF-8")


lazy val main = (project in file("."))
    .settings(
        buildSettings,
        resolvers := commonResolvers,
        libraryDependencies ++= commonDependencies,
        mainClass in assembly := Some("parallel_simplex.Boot"),
        fork := true
    )