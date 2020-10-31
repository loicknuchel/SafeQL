package fr.loicknuchel.safeql.models

import fr.loicknuchel.safeql.testingutils.BaseSpec

class MultiExceptionSpec extends BaseSpec {
  private val e1 = new Exception("an error")
  private val e2 = new Exception("an other error")
  private val m = MultiException(e1, e2)

  describe("MultiException") {
    it("should carry multiple exceptions") {
      m.getMessage shouldBe "\n  - an error\n  - an other error"
      m.getLocalizedMessage shouldBe "\n  - an error\n  - an other error"
      m.getStackTrace shouldBe e1.getStackTrace
      m.getCause shouldBe e1.getCause
    }
  }
}
