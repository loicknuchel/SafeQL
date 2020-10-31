package fr.loicknuchel.safeql.models

import fr.loicknuchel.safeql.testingutils.BaseSpec

class PageSpec extends BaseSpec {
  describe("Page") {
    describe("Params") {
      it("should have all default values") {
        Page.Params().page shouldBe 1 // testing the empty constructor
      }
      it("should have setters") {
        val p = Page.Params()

        p.page shouldBe 1
        p.page(2).page shouldBe 2

        p.pageSize shouldBe 20
        p.pageSize(2).pageSize shouldBe 2

        p.search shouldBe None
        p.search("q").search shouldBe Some("q")

        p.orderBy shouldBe List()
        p.orderBy("name,date", "score").orderBy shouldBe List("name", "date", "score")

        p.filters shouldBe Map()
        p.filters("type" -> "meetup", "future" -> "true").filters shouldBe Map("type" -> "meetup", "future" -> "true")

        p.nullsFirst shouldBe false
        p.withNullsFirst.nullsFirst shouldBe true
      }
      it("should clean arguments on orderBy setter") {
        val p = Page.Params()
        p.orderBy("a,,b ,c", "d,e").orderBy shouldBe List("a", "b", "c", "d", "e")
      }
      it("should update order by only when empty") {
        val p = Page.Params()
        p.defaultOrderBy("b").orderBy shouldBe List("b")
        p.orderBy("a").defaultOrderBy("b").orderBy shouldBe List("a")
      }
    }
  }
}
