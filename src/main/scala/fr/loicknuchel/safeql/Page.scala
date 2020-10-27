package fr.loicknuchel.safeql

import cats.data.NonEmptyList
import fr.loicknuchel.safeql.Page.Params

case class Page[A](items: List[A], params: Params, total: Long)

object Page {

  case class Params(page: Int = 1,
                    pageSize: Long = 20,
                    search: Option[String] = None,
                    orderBy: List[String] = List(),
                    filters: Map[String, String] = Map(),
                    nullsFirst: Boolean = false) {
    val offset: Long = (page - 1) * pageSize
    val orderByNel: Option[NonEmptyList[String]] = NonEmptyList.fromList(orderBy)

    def page(p: Int): Params = copy(page = p)

    def pageSize(s: Long): Params = copy(pageSize = s)

    def search(q: String): Params = copy(search = Some(q))

    def orderBy(o: String*): Params = copy(orderBy = o.flatMap(_.split(",")).toList.map(_.trim).filter(_.nonEmpty))

    def defaultOrderBy(fields: String*): Params = if (orderBy.isEmpty) orderBy(fields: _*) else this

    def filters(f: Map[String, String]): Params = copy(filters = f)

    def filters(f: (String, String)*): Params = filters(f.toMap)
  }

}
