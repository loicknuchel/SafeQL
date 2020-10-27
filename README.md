# SafeQL

A Scala DSL to build type-safe SQL queries on top of Doobie.

## Impatient Quickstart

To use SafeQL in an existing SBT project with Scala 2.11 or a later version, add the following dependency to your `build.sbt`:

```scala
libraryDependencies += "fr.loicknuchel" %% "safeql" % "<version>"
```

- generate tables from db
    - specify SQL files or db connection
    - specify local project and package (default: `src/main/scala` folder and `safeql` package)
- use tables to query the db
