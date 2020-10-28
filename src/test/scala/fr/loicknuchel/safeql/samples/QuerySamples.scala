package fr.loicknuchel.safeql.samples

import doobie.implicits.legacy.instant.JavaTimeInstantMeta // needed to decode Instant
import fr.loicknuchel.safeql.testingutils.Entities.{Post, User}
import fr.loicknuchel.safeql.testingutils.SqlSpec
import fr.loicknuchel.safeql.testingutils.database.Tables.{POSTS, USERS}

class QuerySamples extends SqlSpec {
  it("should perform a basic select") {
    val users: List[User] = USERS.select.all[User].run(xa).unsafeRunSync()
    users shouldBe User.all
    val posts = POSTS.select.all[Post].run(xa).unsafeRunSync()
    posts shouldBe Post.all
  }
  it("should perform crud operations") {
    val id = User.Id(4)
    USERS.insert.values(id, "Lou", "lou@mail.com").run(xa).unsafeRunSync()
    USERS.select.where(_.ID is id).one[User].run(xa).unsafeRunSync() shouldBe User(id, "Lou", Some("lou@mail.com"))
    USERS.update.set(_.NAME, "LouLou").set(_.EMAIL, "loulou@mail.com").where(_.ID is id).run(xa).unsafeRunSync()
    USERS.select.where(_.ID is id).one[User].run(xa).unsafeRunSync() shouldBe User(id, "LouLou", Some("loulou@mail.com"))
    USERS.delete.where(_.ID is id).run(xa).unsafeRunSync()
    USERS.select.where(_.ID is id).option[User].run(xa).unsafeRunSync() shouldBe None
  }
  it("should join tables safely") {
    val postsWithUsers: List[(Post, User)] = POSTS.joinOn(_.AUTHOR).select.all[(Post, User)].run(xa).unsafeRunSync()
    val expected = Post.all.map(p => p -> User.all.find(_.id == p.author).get)
    postsWithUsers shouldBe expected
  }
}
