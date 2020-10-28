package fr.loicknuchel.safeql.gen.reader

import cats.effect.IO
import fr.loicknuchel.safeql.gen.Database

trait Reader {
  def read(): IO[Database]
}
