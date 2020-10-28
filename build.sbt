name := "SafeQL"
version := "0.1.0"

// see https://www.scala-sbt.org/1.x/docs/Cross-Build.html
// can't build for 2.11 as doobie is no longer available for scala 2.11
val supportedScalaVersions = List("2.13.3", "2.12.12")
scalaVersion := supportedScalaVersions.head
crossScalaVersions := supportedScalaVersions

libraryDependencies ++= Seq(
  "org.tpolecat" %% "doobie-core" % "0.9.2",
  "org.tpolecat" %% "doobie-h2" % "0.9.2",
  "org.flywaydb" % "flyway-core" % "7.1.0",
  "org.slf4j" % "slf4j-api" % "1.7.30",
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "org.scalatest" %% "scalatest" % "3.2.2" % Test)

// option added by addSbtPlugin("io.github.davidgregory084" % "sbt-tpolecat" % "0.1.14")
// but as I have an unused import in generated file, I can't remove it and I can't ignore one package for this option :(
scalacOptions --= Seq("-Wunused:imports", "-Ywarn-unused:imports") // 2.13 and 2.12 options
scalacOptions --= Seq("-Xfatal-warnings") // removed as Extensions classes have warnings due to cross compile, need to find a way to handle them
