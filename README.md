# SafeQL [![Build Status](https://travis-ci.com/loicknuchel/SafeQL.svg?branch=master)](https://travis-ci.com/loicknuchel/SafeQL)

A Scala DSL to build typesafe SQL queries on top of Doobie.

## Quick Start

Add the dependency to your `build.sbt` (Scala 2.11 or later):

```scala
libraryDependencies += "fr.loicknuchel" %% "safeql" % "<version>"
```

Then you can generate table classes from your db, for example with Flyway but you can also use a real database connection, or a list of SQL files:

```scala
import fr.loicknuchel.safeql.gen.Generator
import fr.loicknuchel.safeql.gen.writer.ScalaWriter

object FlywaySample {
  def main(args: Array[String]): Unit = {
    Generator
      .flyway("classpath:sql_migrations")
      .writer(ScalaWriter(packageName = "com.company.db"))
      .generate().unsafeRunSync()
  }
}
```

Once you generated the tables, you have a typesafe model of your database. Use it to perform queries:

```scala
USERS.insert.values(User.Id(4), "Lou", "lou@mail.com").run(xa).unsafeRunSync()
USERS.update.set(_.NAME, "LouLou").set(_.EMAIL, "loulou@mail.com").where(_.ID is User.Id(4)).run(xa).unsafeRunSync()
USERS.delete.where(_.ID is User.Id(4)).run(xa).unsafeRunSync()

val user: Option[User] = USERS.select.where(_.ID is User.Id(1)).option[User].run(xa).unsafeRunSync()
val users: List[User] = USERS.select.all[User].run(xa).unsafeRunSync()
val postsWithUsers: List[(Post, User)] = POSTS.joinOn(_.AUTHOR).select.all[(Post, User)].run(xa).unsafeRunSync()
```
