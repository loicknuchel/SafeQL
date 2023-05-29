name := "SafeQL"
description := "A Scala DSL to build typesafe SQL queries on top of Doobie"
// version := "0.1.0-SNAPSHOT" set by 'sbt-dynver' plugin, brought in by 'sbt-ci-release' plugin

// see https://www.scala-sbt.org/1.x/docs/Cross-Build.html
// can't build for 2.11 as doobie is no longer available for scala 2.11
scalaVersion := "2.12.12"
crossScalaVersions := List("2.12.12", "2.13.3")

// publishing informations
organization := "fr.loicknuchel"
homepage := Some(url("https://github.com/loicknuchel/SafeQL"))
licenses += ("MIT", url("https://opensource.org/licenses/mit-license.php"))
developers := List(Developer("loicknuchel", "Loïc Knuchel", "loicknuchel@gmail.com", url("https://loicknuchel.fr")))
scmInfo := Some(ScmInfo(url("https://github.com/loicknuchel/SafeQL"), "git@github.com:loicknuchel/SafeQL.git"))

libraryDependencies ++= Seq(
  "org.tpolecat" %% "doobie-core" % "0.9.2",
  "org.tpolecat" %% "doobie-h2" % "0.9.2", // for generator
  "org.flywaydb" % "flyway-core" % "7.1.0", // for generator
  "com.github.pureconfig" %% "pureconfig" % "0.14.0", // for CLI
  "org.slf4j" % "slf4j-api" % "1.7.30",
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "org.scalatest" %% "scalatest" % "3.2.16" % Test)

// option added by addSbtPlugin("io.github.davidgregory084" % "sbt-tpolecat" % "0.1.14")
// but as I have an unused import in generated file, I can't remove it and I can't ignore one package for this option :(
scalacOptions --= Seq("-Wunused:imports", "-Ywarn-unused:imports") // 2.13 and 2.12 options
scalacOptions --= Seq("-Xfatal-warnings") // removed as Extensions classes have warnings due to cross compile, need to find a way to handle them

// publishing documentation site
enablePlugins(MicrositesPlugin)
micrositeUrl := "https://loicknuchel.fr"
micrositeBaseUrl := "/SafeQL/"
micrositeAuthor := "Loïc Knuchel"
micrositeHomepage := "https://loicknuchel.fr/SafeQL/"
micrositeOrganizationHomepage := "https://loicknuchel.fr"
micrositeTwitterCreator := "@loicknuchel"
micrositeGithubOwner := "loicknuchel"
micrositeGithubRepo := "SafeQL"
micrositePushSiteWith := GitHub4s
micrositeGithubToken := sys.env.get("GITHUB_TOKEN")
micrositeTheme := "light"
micrositePalette := Map(
  "brand-primary" -> "#6d56c1",
  "brand-secondary" -> "#4f9188",
  "white-color" -> "#ffffff")

// build CLI tool
enablePlugins(PackPlugin)
packMain := Map("safeql" -> "fr.loicknuchel.safeql.gen.Cli")
