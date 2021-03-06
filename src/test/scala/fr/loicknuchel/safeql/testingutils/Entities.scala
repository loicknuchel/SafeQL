package fr.loicknuchel.safeql.testingutils

import java.time.{Instant, LocalDate}

object Entities {

  case class User(id: User.Id,
                  name: String,
                  email: Option[String])

  object User {

    case class Id(value: Int) extends AnyVal

    val loic: User = User(Id(1), "loic", Some("loic@mail.com"))
    val jean: User = User(Id(2), "jean", None)
    val tim: User = User(Id(3), "tim", Some("tim@mail.com"))

    val all = List(loic, jean, tim)
  }

  case class Category(id: Category.Id,
                      name: String)

  object Category {

    case class Id(value: Int) extends AnyVal

    val tech: Category = Category(Id(1), "Tech")
    val political: Category = Category(Id(2), "Political")

    val all = List(tech, political)
  }

  case class Post(id: Post.Id,
                  title: String,
                  text: String,
                  date: Instant,
                  author: User.Id,
                  category: Option[Int])

  object Post {

    case class Id(value: Int) extends AnyVal

    val newYear: Post = Post(Id(1), "Happy new year", "The awful year", Instant.parse("2019-12-31T23:59:00Z"), User.Id(1), None)
    val first2020: Post = Post(Id(2), "First 2020 post", "bla bla", Instant.parse("2020-01-01T12:00:00Z"), User.Id(1), None)
    val sqlQueries: Post = Post(Id(3), "SQL Queries", "Using jOOQ and Doobie", Instant.parse("2020-07-18T16:32:00Z"), User.Id(2), Some(1))

    val all = List(newYear, first2020, sqlQueries)
  }

  case class Kind(char: String, varchar: String, timestamp: Instant, date: LocalDate, boolean: Boolean, int: Int, smallint: Short, bigint: Long, double: Double, a_long_name: Int)

  object Kind {
    val one: Kind = Kind("char", "varchar", Instant.ofEpochSecond(1596615600), LocalDate.of(2020, 8, 5), boolean = true, 1, 4, 10, 4.5, 0)

    val all = List(one)
  }

}
