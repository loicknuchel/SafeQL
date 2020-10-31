package fr.loicknuchel.safeql

import cats.data.NonEmptyList
import doobie.syntax.string._
import fr.loicknuchel.safeql.testingutils.BaseSpec
import fr.loicknuchel.safeql.testingutils.database.Tables.{POSTS, USERS}

class FieldSpec extends BaseSpec {
  private val field = USERS.NAME
  private val field2 = POSTS.TITLE
  private val q = USERS.select.fields(USERS.NAME).option[String]

  describe("Field") {
    it("should generate sql for SqlField") {
      val f1 = SqlFieldRaw(POSTS, "title", POSTS.TITLE.info, None)
      f1.fr.query.sql shouldBe "p.title"
      f1.value.query.sql shouldBe "p.title"
      f1.ref.query.sql shouldBe "p.title"
      val f2 = SqlFieldRaw(POSTS, "title", POSTS.TITLE.info, Some("t"))
      f2.fr.query.sql shouldBe "p.title as t"
      f2.value.query.sql shouldBe "p.title"
      f2.ref.query.sql shouldBe "p.title"
    }
    it("should generate sql for SqlFieldRef") {
      val f1 = SqlFieldRef(POSTS, "author", POSTS.AUTHOR.info, None, USERS.ID)
      f1.fr.query.sql shouldBe "p.author"
      f1.value.query.sql shouldBe "p.author"
      f1.ref.query.sql shouldBe "p.author"
      val f2 = SqlFieldRef(POSTS, "author", POSTS.AUTHOR.info, Some("a"), USERS.ID)
      f2.fr.query.sql shouldBe "p.author as a"
      f2.value.query.sql shouldBe "p.author"
      f2.ref.query.sql shouldBe "p.author"
    }
    it("should generate sql for TableField") {
      val f1 = TableField("name", Some("u"), Some("n"))
      f1.fr.query.sql shouldBe "u.name as n"
      f1.value.query.sql shouldBe "u.name"
      f1.ref.query.sql shouldBe "n"
      val f2 = TableField("name", Some("u"), None)
      f2.fr.query.sql shouldBe "u.name"
      f2.value.query.sql shouldBe "u.name"
      f2.ref.query.sql shouldBe "u.name"
      val f3 = TableField("name", None, Some("n"))
      f3.fr.query.sql shouldBe "name as n"
      f3.value.query.sql shouldBe "name"
      f3.ref.query.sql shouldBe "n"
      val f4 = TableField("name", None, None)
      f4.fr.query.sql shouldBe "name"
      f4.value.query.sql shouldBe "name"
      f4.ref.query.sql shouldBe "name"
    }
    it("should generate sql for NullField") {
      val f1 = NullField("name")
      f1.fr.query.sql shouldBe "null as name"
      f1.value.query.sql shouldBe "null"
      f1.ref.query.sql shouldBe "name"
    }
    it("should generate sql for QueryField") {
      val f1 = QueryField(q, Some("n"))
      f1.fr.query.sql shouldBe "(SELECT u.name FROM users u) as n"
      f1.value.query.sql shouldBe "(SELECT u.name FROM users u)"
      f1.ref.query.sql shouldBe "n"
      val f2 = QueryField(q, None)
      f2.fr.query.sql shouldBe "(SELECT u.name FROM users u)"
      f2.value.query.sql shouldBe "(SELECT u.name FROM users u)"
      f2.ref.query.sql shouldBe "SELECT u.name FROM users u"
    }
    it("should generate sql for SimpleAggField") {
      val f1 = SimpleAggField("name", Some("n"))
      f1.fr.query.sql shouldBe "name as n"
      f1.value.query.sql shouldBe "name"
      f1.ref.query.sql shouldBe "n"
      val f2 = SimpleAggField("name", None)
      f2.fr.query.sql shouldBe "name"
      f2.value.query.sql shouldBe "name"
      f2.ref.query.sql shouldBe "name"
    }
    it("should generate sql for QueryAggField") {
      val f1 = QueryAggField(q, Some("n"))
      f1.fr.query.sql shouldBe "(SELECT u.name FROM users u) as n"
      f1.value.query.sql shouldBe "(SELECT u.name FROM users u)"
      f1.ref.query.sql shouldBe "n"
      val f2 = QueryAggField(q, None)
      f2.fr.query.sql shouldBe "(SELECT u.name FROM users u)"
      f2.value.query.sql shouldBe "(SELECT u.name FROM users u)"
      f2.ref.query.sql shouldBe "SELECT u.name FROM users u"
    }
    it("should build cond") {
      field.is("lou") shouldBe Cond.IsValue(field, "lou")
      field.isNot("lou") shouldBe Cond.IsNotValue(field, "lou")
      field.is(field2) shouldBe Cond.IsField(field, field2)
      field.is(q) shouldBe Cond.IsQuery(field, q)
      field.like("%lou%") shouldBe Cond.Like(field, "%lou%")
      field.notLike("%lou%") shouldBe Cond.NotLike(field, "%lou%")
      field.ilike("%lou%") shouldBe Cond.ILike(field, "%lou%")
      field.gt("lou") shouldBe Cond.GtValue(field, "lou")
      field.gte("lou") shouldBe Cond.GteValue(field, "lou")
      field.lt("lou") shouldBe Cond.LtValue(field, "lou")
      field.lte("lou") shouldBe Cond.LteValue(field, "lou")
      field.isNull shouldBe Cond.IsNull(field)
      field.notNull shouldBe Cond.NotNull(field)
      field.in(NonEmptyList.of("lou")) shouldBe Cond.InValues(field, NonEmptyList.of("lou"))
      field.notIn(NonEmptyList.of("lou")) shouldBe Cond.NotInValues(field, NonEmptyList.of("lou"))
      field.in(q) shouldBe Cond.InQuery(field, q)
      field.notIn(q) shouldBe Cond.NotInQuery(field, q)
      field.cond(fr0" LIKE 'lou'").sql shouldBe Cond.CustomCond(field, fr0" LIKE 'lou'").sql // as Fragments have identity equality
    }
    it("should build expr") {
      field.lower shouldBe Expr.Lower(Expr.ValueField(field))
    }
    it("should be aliased") {
      USERS.NAME.alias shouldBe None
      USERS.NAME.as("n").alias shouldBe Some("n")

      POSTS.AUTHOR.alias shouldBe None
      POSTS.AUTHOR.as("a").alias shouldBe Some("a")

      TableField("name").as("n") shouldBe TableField("name", alias = Some("n"))
      NullField("name").as("n") shouldBe NullField("n")
      QueryField(q).as("n") shouldBe QueryField(q, alias = Some("n"))
      SimpleAggField("name").as("n") shouldBe SimpleAggField("name", alias = Some("n"))
      QueryAggField(q).as("n") shouldBe QueryAggField(q, alias = Some("n"))
    }
    describe("SqlField") {
      it("should create null fields") {
        USERS.NAME.asNull shouldBe NullField("name")
        USERS.NAME.as("n").asNull shouldBe NullField("n")
        USERS.NAME.asNull("n") shouldBe NullField("n")
      }
    }
    describe("AggField") {
      it("should have many constructors") {
        AggField("name") shouldBe SimpleAggField("name")
        AggField("name", "n") shouldBe SimpleAggField("name", Some("n"))
        AggField(q) shouldBe QueryAggField(q)
        AggField(q, "n") shouldBe QueryAggField(q, Some("n"))
      }
    }
    describe("Order") {
      it("should have many constructors") {
        Field.Order(field, asc = true) shouldBe Field.Order(field, asc = true, None)
        Field.Order("id") shouldBe Field.Order(TableField("id"), asc = true, None)
        Field.Order("-id") shouldBe Field.Order(TableField("id"), asc = false, None)
        Field.Order("id", Some("u")) shouldBe Field.Order(TableField("id", Some("u")), asc = true, None)
      }
      it("should build order with nulls last") {
        Field.Order(TableField("name"), asc = true, None).fr(nullsFirst = false).query.sql shouldBe "name IS NULL, name"
        // FIXME Field.Order(TableField("null").as("name"), asc = true, None).fr(nullsFirst = false).query.sql shouldBe "name IS NULL, name"
        Field.Order(TableField("name"), asc = false, None).fr(nullsFirst = false).query.sql shouldBe "name IS NULL, name DESC"
      }
      it("should build order with nulls first") {
        Field.Order(TableField("name"), asc = true, None).fr(nullsFirst = true).query.sql shouldBe "name IS NOT NULL, name"
      }
    }
  }
}
