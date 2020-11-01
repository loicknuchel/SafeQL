package fr.loicknuchel.safeql

import java.time.Instant

import cats.data.NonEmptyList
import doobie.util.meta.Meta
import fr.loicknuchel.safeql.models.{ConflictingTableFields, NotImplementedJoin, UnknownTableFields}
import fr.loicknuchel.safeql.testingutils.BaseSpec
import fr.loicknuchel.safeql.testingutils.Entities.{Category, Post, User}
import fr.loicknuchel.safeql.testingutils.database.Tables.{CATEGORIES, FEATURED, POSTS, USERS}

class TableSpec extends BaseSpec {
  protected implicit val instantMeta: Meta[Instant] = doobie.implicits.legacy.instant.JavaTimeInstantMeta

  describe("Table") {
    it("should find a field") {
      POSTS.field[String]("id") shouldBe POSTS.ID
      POSTS.id[String] shouldBe POSTS.ID // use select dynamic \o/
      an[UnknownTableFields[_]] should be thrownBy POSTS.field("miss")

      val joined = POSTS.joinOn(_.AUTHOR)
      joined.field("title") shouldBe POSTS.TITLE
      joined.field("name") shouldBe USERS.NAME
      joined.name shouldBe USERS.NAME
      an[UnknownTableFields[_]] should be thrownBy joined.field("miss")
      an[ConflictingTableFields[_]] should be thrownBy POSTS.joinOn(_.AUTHOR).field("id")

      val unioned = POSTS.select.fields(POSTS.ID, POSTS.TITLE.as("name")).union(USERS.select.fields(USERS.ID, USERS.NAME))
      unioned.field("id") shouldBe TableField("id")
      unioned.id shouldBe TableField("id")
      an[UnknownTableFields[_]] should be thrownBy unioned.field("miss")
    }
    it("should check for a field presence") {
      POSTS.has(POSTS.ID) shouldBe true
      POSTS.has(USERS.ID) shouldBe false
      POSTS.has(TableField("id")) shouldBe false
      // FIXME POSTS.dropFields(POSTS.ID).has(POSTS.ID) shouldBe false // because `has` check just schema & table name

      val unioned = POSTS.select.fields(POSTS.ID, POSTS.TITLE.as("name")).union(USERS.select.fields(USERS.ID, USERS.NAME))
      unioned.has(TableField("name")) shouldBe true
      unioned.has(TableField("miss")) shouldBe false
      unioned.has(NullField("name")) shouldBe false
    }
    it("should manipulate fields") {
      USERS.dropFields(_.name != "id").select.all[User.Id].fields shouldBe List(USERS.ID)
      USERS.dropFields(List(USERS.NAME, USERS.EMAIL)).select.all[User.Id].fields shouldBe List(USERS.ID)
      USERS.dropFields(USERS.NAME, USERS.EMAIL).select.all[User.Id].fields shouldBe List(USERS.ID)

      val joined = POSTS.joinOn(_.AUTHOR)
      joined.dropFields(_.name != "id").select.all[(Post.Id, User.Id)].fields shouldBe List(POSTS.ID, USERS.ID)
      joined.dropFields(USERS.ID, USERS.NAME, USERS.EMAIL).select.all[Post].fields shouldBe POSTS.getFields
    }
    describe("join") {
      it("should join two sql tables") {
        POSTS.join(USERS).on(POSTS.AUTHOR.is(USERS.ID)).sql shouldBe "posts p INNER JOIN users u ON p.author=u.id"
        POSTS.join(USERS).on(POSTS.AUTHOR is USERS.ID).sql shouldBe "posts p INNER JOIN users u ON p.author=u.id"
        POSTS.join(USERS).on(POSTS.AUTHOR is _.ID).sql shouldBe "posts p INNER JOIN users u ON p.author=u.id"
        POSTS.join(USERS).on(_.AUTHOR is _.ID).sql shouldBe "posts p INNER JOIN users u ON p.author=u.id"
      }
      it("should specify join kind") {
        POSTS.join(USERS, _.LeftOuter).on(POSTS.AUTHOR.is(USERS.ID)).sql shouldBe "posts p LEFT OUTER JOIN users u ON p.author=u.id"
      }
      it("should join all kind of tables") {
        val POSTS_WITH_CATEGORIES = POSTS.join(CATEGORIES).on(_.CATEGORY is _.ID)
        val FEATURED_WITH_USERS = FEATURED.join(USERS).on(_.BY is _.ID)
        val COMMON_NAMES = USERS.select.withFields(_.NAME).union(CATEGORIES.select.withFields(_.NAME).orderBy())

        POSTS_WITH_CATEGORIES.sql shouldBe "posts p INNER JOIN categories c ON p.category=c.id"
        POSTS_WITH_CATEGORIES.join(USERS).on(POSTS.AUTHOR is _.ID).sql shouldBe "posts p INNER JOIN categories c ON p.category=c.id INNER JOIN users u ON p.author=u.id"
        FEATURED.join(POSTS_WITH_CATEGORIES).on((f, _) => f.POST_ID is POSTS.ID).sql shouldBe "featured INNER JOIN posts p ON featured.post_id=p.id INNER JOIN categories c ON p.category=c.id"
        FEATURED_WITH_USERS.join(POSTS_WITH_CATEGORIES).on(FEATURED.POST_ID is POSTS.ID).sql shouldBe "featured INNER JOIN users u ON featured.by=u.id INNER JOIN posts p ON featured.post_id=p.id INNER JOIN categories c ON p.category=c.id"
        an[NotImplementedJoin[_, _]] should be thrownBy USERS.join(COMMON_NAMES).on(_.NAME is _.name[String])
        an[NotImplementedJoin[_, _]] should be thrownBy FEATURED_WITH_USERS.join(COMMON_NAMES).on(_.name[String] is _.name[String])
        an[NotImplementedJoin[_, _]] should be thrownBy COMMON_NAMES.join(USERS).on(_.name[String] is _.NAME)
        an[NotImplementedJoin[_, _]] should be thrownBy COMMON_NAMES.join(FEATURED_WITH_USERS).on(_.name[String] is _.name[String])
        an[NotImplementedJoin[_, _]] should be thrownBy COMMON_NAMES.join(COMMON_NAMES).on(_.name[String] is _.name[String])
      }
      it("should manipulate fields") {
        val joined = POSTS.joinOn(_.AUTHOR)
        joined.getFields shouldBe POSTS.getFields ++ USERS.getFields
        joined.fields(POSTS.getFields).getFields shouldBe POSTS.getFields
        joined.fields(POSTS.ID).getFields shouldBe List(POSTS.ID)
        joined.dropFields(_.name != "id").getFields shouldBe List(POSTS.ID, USERS.ID)
        joined.dropFields(USERS.ID, USERS.NAME, USERS.EMAIL).getFields shouldBe POSTS.getFields
        joined.dropFields(USERS.getFields).getFields shouldBe POSTS.getFields
        joined.dropFields(USERS.getFields).addFields(USERS.ID).getFields shouldBe POSTS.getFields :+ USERS.ID
      }
      it("should update sorts") {
        val s = Table.Sort(POSTS.TITLE.asc)
        val joined = POSTS.joinOn(_.AUTHOR)
        joined.getSorts shouldBe List()
        joined.sorts(s).getSorts shouldBe List(s)
      }
      it("should update filters") {
        val f = Table.Filter.Bool.fromNullable("email", "Has email", USERS.EMAIL)
        val joined = POSTS.joinOn(_.AUTHOR)
        joined.getFilters shouldBe List()
        joined.filters(f).getFilters shouldBe List(f)
      }
      describe("auto") {
        it("should automatically join tables using ref fields") {
          POSTS.joinOn(_.AUTHOR, _.Inner).sql shouldBe "posts p INNER JOIN users u ON p.author=u.id"
        }
        it("should perform auto joins with incoming ref field") {
          USERS.joinOn(POSTS.AUTHOR, _.Inner).sql shouldBe "users u INNER JOIN posts p ON u.id=p.author"
        }
        it("should choose auto join based on field kind") {
          POSTS.joinOn(POSTS.AUTHOR).sql shouldBe "posts p INNER JOIN users u ON p.author=u.id"
          POSTS.joinOn(POSTS.CATEGORY).sql shouldBe "posts p LEFT OUTER JOIN categories c ON p.category=c.id"
          POSTS.joinOn(_.AUTHOR).sql shouldBe "posts p INNER JOIN users u ON p.author=u.id"
          POSTS.joinOn(_.CATEGORY).sql shouldBe "posts p LEFT OUTER JOIN categories c ON p.category=c.id"
        }
      }
    }
    describe("union") {
      it("should join two queries") {
        val q1 = POSTS.select.fields(POSTS.ID, POSTS.TITLE.as("name"))
        val q2 = USERS.select.fields(USERS.ID, USERS.NAME)
        val unioned = q1.union(q2, sorts = List(("name", "name", List("-name"))), search = List("name"))
        unioned.select.all[(String, String)].sql shouldBe "SELECT id, name FROM ((SELECT p.id, p.title as name FROM posts p) UNION (SELECT u.id, u.name FROM users u)) ORDER BY name IS NULL, name DESC"
      }
      it("should fail when queries do not have the same number of fields") {
        val q1 = POSTS.select.fields(POSTS.ID, POSTS.TITLE)
        val q2 = USERS.select.fields(USERS.ID)
        an[Exception] should be thrownBy q1.union(q2)
      }
      it("should fail when queries do not have the same name of fields") {
        val q1 = POSTS.select.fields(POSTS.ID, POSTS.TITLE)
        val q2 = USERS.select.fields(USERS.ID, USERS.NAME)
        an[Exception] should be thrownBy q1.union(q2)
      }
      it("should fail when multiple fields have the same name") {
        val q1 = POSTS.select.fields(POSTS.ID, POSTS.TITLE.as("id"))
        val q2 = USERS.select.fields(USERS.ID, USERS.NAME.as("id"))
        an[Exception] should be thrownBy q1.union(q2)
      }
      it("should fail when a sort have an empty list") {
        val q1 = POSTS.select.fields(POSTS.ID, POSTS.TITLE.as("name"))
        val q2 = USERS.select.fields(USERS.ID, USERS.NAME)
        an[Exception] should be thrownBy q1.union(q2, sorts = List(("name", "name", List())))
      }
      it("should fail when a sort have an unknown field") {
        val q1 = POSTS.select.fields(POSTS.ID, POSTS.TITLE.as("name"))
        val q2 = USERS.select.fields(USERS.ID, USERS.NAME)
        an[Exception] should be thrownBy q1.union(q2, sorts = List(("name", "name", List("miss"))))
      }
      it("should fail when search has an unknown field") {
        val q1 = POSTS.select.fields(POSTS.ID, POSTS.TITLE.as("name"))
        val q2 = USERS.select.fields(USERS.ID, USERS.NAME)
        an[Exception] should be thrownBy q1.union(q2, search = List("miss"))
      }
      it("should update sorts") {
        val s = Table.Sort(POSTS.TITLE.asc)
        val unioned = POSTS.select.fields(POSTS.ID, POSTS.TITLE.as("name")).union(USERS.select.fields(USERS.ID, USERS.NAME))
        unioned.getSorts shouldBe List()
        unioned.sorts(s).getSorts shouldBe List(s)
      }
      it("should update filters") {
        val f = Table.Filter.Bool.fromNullable("email", "Has email", USERS.EMAIL)
        val unioned = POSTS.select.fields(POSTS.ID, POSTS.TITLE.as("name")).union(USERS.select.fields(USERS.ID, USERS.NAME))
        unioned.getFilters shouldBe List()
        unioned.filters(f).getFilters shouldBe List(f)
      }
    }
    describe("Sort") {
      it("should have many constructors") {
        Table.Sort(USERS.NAME.asc) shouldBe Table.Sort("name", "name", NonEmptyList.of(USERS.NAME.asc))
        Table.Sort("Name", USERS.NAME.asc) shouldBe Table.Sort("name", "Name", NonEmptyList.of(USERS.NAME.asc))
        Table.Sort("n", "Name", USERS.NAME.asc) shouldBe Table.Sort("n", "Name", NonEmptyList.of(USERS.NAME.asc))
      }
    }
    describe("Filter") {
      it("should create a Bool filter") {
        implicit val ctx: Query.Ctx = Query.Ctx.Basic(Instant.now())
        Table.Filter.Bool.fromNullable(
          key = "email",
          label = "Has email",
          field = USERS.EMAIL
        ).filter("true") shouldBe Some(USERS.EMAIL.notNull)

        Table.Filter.Bool.fromCount(
          key = "users",
          label = "Has users",
          field = USERS.ID
        ).filter("true") shouldBe Some(AggField(s"COALESCE(COUNT(DISTINCT ${USERS.ID.sql}), 0)").gt(0))

        Table.Filter.Bool.fromNow(
          key = "now",
          label = "Is now",
          startField = FEATURED.START,
          endField = FEATURED.STOP
        ).filter("true") shouldBe Some(FEATURED.START.lt(ctx.now) and FEATURED.STOP.gt(ctx.now))
      }
      it("should build an Enum filter") {
        implicit val ctx: Query.Ctx = Query.Ctx.Basic(Instant.now())
        Table.Filter.Enum.fromValues(
          key = "category",
          label = "Category",
          field = POSTS.CATEGORY,
          values = List(("tech", "Tech", Category.tech.id))
        ).filter("tech") shouldBe Some(POSTS.CATEGORY.is(Category.tech.id))
      }
      it("should build an Value filter") {
        implicit val ctx: Query.Ctx = Query.Ctx.Basic(Instant.now())
        Table.Filter.Value.fromField(
          key = "name",
          label = "Name",
          field = USERS.NAME
        ).filter("lo") shouldBe Some(USERS.NAME.ilike("%lo%"))
      }
    }
  }
}
