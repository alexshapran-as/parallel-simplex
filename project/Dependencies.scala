import sbt._

object Dependencies {
  private val akkaVersion = "2.5.26"
  private val akkaHttpVersion = "10.1.11"
  private val logVersion = "2.13.3"

  val akkaActor = "com.typesafe.akka" %% "akka-actor" % akkaVersion
  val akkaRemote = "com.typesafe.akka" %% "akka-remote" % akkaVersion
  val akkaSlf4j = "com.typesafe.akka" %% "akka-slf4j" % akkaVersion
  val akkaStream = "com.typesafe.akka" %% "akka-stream" % akkaVersion
  val akkaHttp = "com.typesafe.akka" %% "akka-http" % akkaHttpVersion
  val akkaHttpSession = "com.softwaremill.akka-http-session" %% "core" % "0.5.10"

  val json4s = "org.json4s" %% "json4s-native" % "3.6.5"

  val aparAPI = "com.aparapi" % "aparapi" % "2.0.0"

  val log4jApi = "org.apache.logging.log4j" % "log4j-api" % logVersion
  val log4jCore = "org.apache.logging.log4j" % "log4j-core" % logVersion
  val logback = "ch.qos.logback" % "logback-classic" % "1.2.3"
  val logstashLogback = "net.logstash.logback" % "logstash-logback-encoder" % "5.3"

  def commonDependencies = Seq(
    akkaActor, akkaRemote, akkaHttp, akkaStream, akkaSlf4j, akkaHttpSession,
    json4s,
    aparAPI,
    log4jApi, log4jCore, logback, logstashLogback
  )
}
