package fr.loicknuchel.safeql

import fr.loicknuchel.safeql.testingutils.BaseSpec
import fr.loicknuchel.safeql.testingutils.database.Tables.{POSTS, USERS}
import fr.loicknuchel.safeql.testingutils.database.tables.{POSTS, USERS}

class ExprSpec extends BaseSpec {
  private val sqlField: SqlFieldRaw[String, USERS] = USERS.NAME
  private val sqlField2: SqlFieldRaw[String, POSTS] = POSTS.TITLE
  private val query: Query.Select.One[String] = USERS.select.fields(USERS.NAME).where(_.NAME is "lou").one[String]

  describe("Expr") {
    it("should generate SQL") {
      Expr.Value("lou").sql shouldBe "?"
      Expr.ValueField(sqlField).sql shouldBe "u.name"
      Expr.Random().sql shouldBe "RANDOM()"
      Expr.SubQuery(query).sql shouldBe "(SELECT u.name FROM users u WHERE u.name=?)"

      val e1 = Expr.ValueField(sqlField)
      val e2 = Expr.ValueField(sqlField2)
      Expr.Lower(e1).sql shouldBe "LOWER(u.name)"
      Expr.Floor(e1).sql shouldBe "FLOOR(u.name)"
      Expr.Times(e1, e2).sql shouldBe "u.name * p.title"
    }
  }
}
