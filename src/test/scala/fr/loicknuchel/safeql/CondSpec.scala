package fr.loicknuchel.safeql

import cats.data.NonEmptyList
import doobie.syntax.string._
import fr.loicknuchel.safeql.testingutils.BaseSpec
import fr.loicknuchel.safeql.testingutils.database.Tables.{POSTS, USERS}
import fr.loicknuchel.safeql.testingutils.database.tables.{POSTS, USERS}

class CondSpec extends BaseSpec {
  private val sqlField: SqlFieldRaw[String, USERS] = USERS.NAME
  private val expr: Expr = sqlField.lower
  private val query: Query.Select.One[String] = USERS.select.fields(USERS.NAME).where(_.NAME is "lou").one[String]
  private val sqlField2: SqlFieldRaw[String, POSTS] = POSTS.TITLE
  private val sqlFieldRef: SqlFieldRef[String, POSTS, USERS] = POSTS.AUTHOR.asInstanceOf[SqlFieldRef[String, POSTS, USERS]]
  private val tableField: TableField[String] = TableField("name")
  private val nullField: NullField[String] = NullField("name")
  private val queryField: QueryField[String] = QueryField(query)
  private val aggField: SimpleAggField[String] = SimpleAggField("name")
  private val queryAggField: QueryAggField[String] = QueryAggField(query)

  describe("Cond") {
    it("should generate SQL for eq condition and each field") {
      Cond.IsValue(sqlField, "lou").sql shouldBe "u.name=?"
      Cond.IsValue(sqlFieldRef, "lou").sql shouldBe "p.author=?"
      Cond.IsValue(tableField, "lou").sql shouldBe "name=?"
      Cond.IsValue(nullField, "lou").sql shouldBe "null as name=?" // FIXME should be "name=?"
      Cond.IsValue(queryField, "lou").sql shouldBe "(SELECT u.name FROM users u WHERE u.name=?)=?"
      Cond.IsValue(aggField, "lou").sql shouldBe "name=?"
      Cond.IsValue(queryAggField, "lou").sql shouldBe "(SELECT u.name FROM users u WHERE u.name=?)=?"
    }
    it("should generate SQL for sql field and each condition") {
      Cond.IsValue(sqlField, "lou").sql shouldBe "u.name=?"
      Cond.IsNotValue(sqlField, "lou").sql shouldBe "u.name != ?"
      Cond.IsField(sqlField, sqlField2).sql shouldBe "u.name=p.title"
      Cond.IsQuery(sqlField, query).sql shouldBe "u.name=(SELECT u.name FROM users u WHERE u.name=?)"
      Cond.Like(sqlField, "%lou%").sql shouldBe "u.name LIKE ?"
      Cond.NotLike(sqlField, "%lou%").sql shouldBe "u.name NOT LIKE ?"
      Cond.LikeExpr(expr, "%lou%").sql shouldBe "LOWER(u.name) LIKE ?"
      Cond.ILike(sqlField, "%lou%").sql shouldBe "u.name ILIKE ?"
      Cond.GtValue(sqlField, "lou").sql shouldBe "u.name > ?"
      Cond.GteValue(sqlField, "lou").sql shouldBe "u.name >= ?"
      Cond.LtValue(sqlField, "lou").sql shouldBe "u.name < ?"
      Cond.LteValue(sqlField, "lou").sql shouldBe "u.name <= ?"
      Cond.IsNull(sqlField).sql shouldBe "u.name IS NULL"
      Cond.NotNull(sqlField).sql shouldBe "u.name IS NOT NULL"
      Cond.InValues(sqlField, NonEmptyList.of("lou")).sql shouldBe "u.name IN (?)"
      Cond.NotInValues(sqlField, NonEmptyList.of("lou")).sql shouldBe "u.name NOT IN (?)"
      Cond.InQuery(sqlField, query).sql shouldBe "u.name IN (SELECT u.name FROM users u WHERE u.name=?)"
      Cond.NotInQuery(sqlField, query).sql shouldBe "u.name NOT IN (SELECT u.name FROM users u WHERE u.name=?)"
      Cond.CustomCond(sqlField, fr0" LIKE 'lou'").sql shouldBe "u.name LIKE 'lou'"

      val c1 = Cond.IsValue(sqlField, "lou")
      val c2 = Cond.IsValue(sqlField2, "lou")
      Cond.And(c1, c2).sql shouldBe "u.name=? AND p.title=?"
      Cond.Or(c1, c2).sql shouldBe "u.name=? OR p.title=?"
      Cond.Parentheses(c1).sql shouldBe "(u.name=?)"
    }
  }
}
