package org.babysherlock.util

import java.util.regex.Pattern

/**
 * String Utilities
 * @author Angel Chang
 */
object StringUtils {
  private final val camelCasePattern: Pattern = Pattern.compile("([a-z])([A-Z])")
  private final val underscorePattern: Pattern = Pattern.compile("_")
  private final val whitespacePattern: Pattern = Pattern.compile("\\s+")

  // Converts string of the form LivingRoom to Living Room
  def camelCaseToText(cc: String, lowercase: Boolean = false): String = {
    if (cc != null) {
      val s = camelCasePattern.matcher(cc).replaceAll("$1 $2")
      if (lowercase) s.toLowerCase()
      else s
    } else null
  }

  def textToCamelCase(x: String): String = {
    if (x != null)
      x.split("\\s+").map( a => a.capitalize ).mkString("")
    else null
  }

  // Converts string of the form living_room to living room
  def delimitedToText(x: String, delimiterPattern: Pattern = underscorePattern, lowercase: Boolean = false): String = {
    if (x != null) {
      val s = delimiterPattern.matcher(x).replaceAll(" ")
      if (lowercase) s.toLowerCase()
      else s
    } else null
  }

  def textToDelimited(x: String, delimiter: String = "_", lowercase: Boolean = false): String = {
    if (x != null) {
      val s = whitespacePattern.matcher(x).replaceAll(delimiter)
      if (lowercase) s.toLowerCase()
      else s
    } else null
  }
}
