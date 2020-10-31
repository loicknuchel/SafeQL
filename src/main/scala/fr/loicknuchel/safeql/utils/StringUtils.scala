package fr.loicknuchel.safeql.utils

import java.text.Normalizer

private[safeql] object StringUtils {
  def removeDiacritics(str: String): String =
    Normalizer.normalize(str, Normalizer.Form.NFD)
      .replaceAll("\\p{InCombiningDiacriticalMarks}+", "")

  def slugify(str: String): String =
    removeDiacritics(str).trim.toLowerCase()
      .replaceAll("[ _+'\"]", "-")
      .replaceAll("--+", "-")
      .replaceAll("[^a-z0-9-]", "")

  def isScalaPackage(str: String): Boolean = str.matches("[a-z.]+")
}
