package fr.loicknuchel.safeql.gen

import cats.effect.IO
import fr.loicknuchel.safeql.gen.reader.Reader
import fr.loicknuchel.safeql.gen.writer.Writer
import fr.loicknuchel.safeql.utils.Extensions._

object Generator {
  def generate(xa: doobie.Transactor[IO], reader: Reader, writer: Writer): IO[Unit] = for {
    database <- reader.read(xa)
    _ <- writer.write(database).toIO
  } yield ()
}
