package fr.loicknuchel.safeql

import java.sql.Types._

import fr.loicknuchel.safeql.testingutils.BaseSpec

class JdbcTypeSpec extends BaseSpec {
  describe("JdbcType") {
    it("should build type from int") {
      JdbcType.fromInt(ARRAY) shouldBe JdbcType.Array
      JdbcType.fromInt(BIGINT) shouldBe JdbcType.BigInt
      JdbcType.fromInt(BINARY) shouldBe JdbcType.Binary
      JdbcType.fromInt(BIT) shouldBe JdbcType.Bit
      JdbcType.fromInt(BLOB) shouldBe JdbcType.Blob
      JdbcType.fromInt(BOOLEAN) shouldBe JdbcType.Boolean
      JdbcType.fromInt(CHAR) shouldBe JdbcType.Char
      JdbcType.fromInt(CLOB) shouldBe JdbcType.Clob
      JdbcType.fromInt(DATALINK) shouldBe JdbcType.DataLink
      JdbcType.fromInt(DATE) shouldBe JdbcType.Date
      JdbcType.fromInt(DECIMAL) shouldBe JdbcType.Decimal
      JdbcType.fromInt(DISTINCT) shouldBe JdbcType.Distinct
      JdbcType.fromInt(DOUBLE) shouldBe JdbcType.Double
      JdbcType.fromInt(FLOAT) shouldBe JdbcType.Float
      JdbcType.fromInt(INTEGER) shouldBe JdbcType.Integer
      JdbcType.fromInt(JAVA_OBJECT) shouldBe JdbcType.JavaObject
      JdbcType.fromInt(LONGNVARCHAR) shouldBe JdbcType.LongnVarChar
      JdbcType.fromInt(LONGVARBINARY) shouldBe JdbcType.LongVarBinary
      JdbcType.fromInt(LONGVARCHAR) shouldBe JdbcType.LongVarChar
      JdbcType.fromInt(NCHAR) shouldBe JdbcType.NChar
      JdbcType.fromInt(NCLOB) shouldBe JdbcType.NClob
      JdbcType.fromInt(NULL) shouldBe JdbcType.Null
      JdbcType.fromInt(NUMERIC) shouldBe JdbcType.Numeric
      JdbcType.fromInt(NVARCHAR) shouldBe JdbcType.NVarChar
      JdbcType.fromInt(OTHER) shouldBe JdbcType.Other
      JdbcType.fromInt(REAL) shouldBe JdbcType.Real
      JdbcType.fromInt(REF) shouldBe JdbcType.Ref
      JdbcType.fromInt(REF_CURSOR) shouldBe JdbcType.RefCursor
      JdbcType.fromInt(ROWID) shouldBe JdbcType.RowId
      JdbcType.fromInt(SMALLINT) shouldBe JdbcType.SmallInt
      JdbcType.fromInt(SQLXML) shouldBe JdbcType.SqlXml
      JdbcType.fromInt(STRUCT) shouldBe JdbcType.Struct
      JdbcType.fromInt(TIME) shouldBe JdbcType.Time
      JdbcType.fromInt(TIME_WITH_TIMEZONE) shouldBe JdbcType.TimeWithTimezone
      JdbcType.fromInt(TIMESTAMP) shouldBe JdbcType.Timestamp
      JdbcType.fromInt(TIMESTAMP_WITH_TIMEZONE) shouldBe JdbcType.TimestampWithTimezone
      JdbcType.fromInt(TINYINT) shouldBe JdbcType.TinyInt
      JdbcType.fromInt(VARBINARY) shouldBe JdbcType.VarBinary
      JdbcType.fromInt(VARCHAR) shouldBe JdbcType.VarChar
      JdbcType.fromInt(-155) shouldBe JdbcType.MsSqlDateTimeOffset
      JdbcType.fromInt(-150) shouldBe JdbcType.MsSqlVariant
      JdbcType.fromInt(-10) shouldBe JdbcType.NVarChar
      JdbcType.fromInt(123) shouldBe JdbcType.Unknown(123)
    }
  }
}
