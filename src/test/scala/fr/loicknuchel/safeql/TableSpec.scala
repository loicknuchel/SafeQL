package fr.loicknuchel.safeql

import java.time.Instant

import doobie.util.meta.Meta
import fr.loicknuchel.safeql.models.{ConflictingTableFields, NotImplementedJoin, UnknownTableFields}
import fr.loicknuchel.safeql.testingutils.BaseSpec
import fr.loicknuchel.safeql.testingutils.Entities.{Post, User}
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
      // FIXME POSTS.dropFields(POSTS.ID).has(POSTS.ID) shouldBe false // because `has` check just schema & table name
    }
    it("should manipulate fields") {
      USERS.dropFields(_.name != "id").select.all[User.Id].fields shouldBe List(USERS.ID)
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
  }
}
