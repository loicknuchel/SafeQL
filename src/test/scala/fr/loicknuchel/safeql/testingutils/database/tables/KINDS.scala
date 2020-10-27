package fr.loicknuchel.safeql.testingutils.database.tables

import java.time.{Instant, LocalDate}

import fr.loicknuchel.safeql.Table._
import fr.loicknuchel.safeql._
import fr.loicknuchel.safeql.testingutils.Entities._

/**
 * Hello
 *
 * Class generated by fr.loicknuchel.safeql.gen.writer.ScalaWriter
 */
class KINDS private(getAlias: Option[String] = None) extends Table.SqlTable("PUBLIC", "kinds", getAlias) {
  type Self = KINDS

  val CHAR: SqlField[String, KINDS] = SqlField(this, "char", "CHAR(4)", JdbcType.Char, nullable = true, 1)
  val VARCHAR: SqlField[String, KINDS] = SqlField(this, "varchar", "VARCHAR(50)", JdbcType.VarChar, nullable = true, 2)
  val TIMESTAMP: SqlField[Instant, KINDS] = SqlField(this, "timestamp", "TIMESTAMP", JdbcType.Timestamp, nullable = true, 3)
  val DATE: SqlField[LocalDate, KINDS] = SqlField(this, "date", "DATE", JdbcType.Date, nullable = true, 4)
  val BOOLEAN: SqlField[Boolean, KINDS] = SqlField(this, "boolean", "BOOLEAN", JdbcType.Boolean, nullable = true, 5)
  val INT: SqlField[Int, KINDS] = SqlField(this, "int", "INT", JdbcType.Integer, nullable = true, 6)
  val BIGINT: SqlField[Long, KINDS] = SqlField(this, "bigint", "BIGINT", JdbcType.BigInt, nullable = true, 7)
  val DOUBLE: SqlField[Double, KINDS] = SqlField(this, "double", "DOUBLE PRECISION", JdbcType.Double, nullable = true, 8)
  val A_LONG_NAME: SqlField[Int, KINDS] = SqlField(this, "a_long_name", "INT", JdbcType.Integer, nullable = true, 9)

  override def getFields: List[SqlField[_, KINDS]] = List(CHAR, VARCHAR, TIMESTAMP, DATE, BOOLEAN, INT, BIGINT, DOUBLE, A_LONG_NAME)

  override def getSorts: List[Sort] = List()

  override def searchOn: List[SqlField[_, KINDS]] = List(CHAR, VARCHAR, TIMESTAMP, DATE, BOOLEAN, INT, BIGINT, DOUBLE, A_LONG_NAME)

  override def getFilters: List[Filter] = List()

  def alias(alias: String): KINDS = new KINDS(Some(alias))
}

private[database] object KINDS {
  val table = new KINDS() // table instance, should be accessed through `fr.loicknuchel.safeql.testingutils.database.Tables` object
}
