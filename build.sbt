name := "SafeQL"

version := "0.1.0"

scalaVersion := "2.13.3" // 2.12.12 & 2.11.12

libraryDependencies ++= Seq(
  "org.tpolecat" %% "doobie-core" % "0.9.2",
  "org.tpolecat" %% "doobie-h2" % "0.9.2",
  "org.flywaydb" % "flyway-core" % "7.1.0",
  "org.scalatest" %% "scalatest" % "3.2.2" % Test
)

// option added by addSbtPlugin("io.github.davidgregory084" % "sbt-tpolecat" % "0.1.14")
// but as I have an unused import in generated file, I can't remove it and I can't ignore one package for this option :(
scalacOptions --= Seq("-Wunused:imports")
