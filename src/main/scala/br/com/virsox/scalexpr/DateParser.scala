package br.com.virsox.scalexpr

import java.time.Instant

import fastparse._, SingleLineWhitespace._

trait DateParser {
  this: ExpressionParser =>
  // ---------------------------------------------------------------
  // --------------    Date literal parsers    ---------------------
  // ---------------------------------------------------------------
  def yearParser[_: P]: P[Unit]  = P(digits.rep(min = 4, max = 4))
  def monthParser[_: P]: P[Unit] = P(StringIn("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"))
  def dayParser[_: P]: P[Unit] =
    P(
      StringIn(
        "01",
        "02",
        "03",
        "04",
        "05",
        "06",
        "07",
        "08",
        "09",
        "10",
        "11",
        "12",
        "13",
        "14",
        "15",
        "16",
        "17",
        "18",
        "19",
        "20",
        "21",
        "22",
        "23",
        "24",
        "25",
        "26",
        "27",
        "28",
        "29",
        "30",
        "31"
      )
    )

  // months with 31 days
  val months31 = Seq(1, 3, 5, 7, 8, 10, 12)

  // months with 30 days
  val months30 = Seq(4, 6, 9, 11)

  // parses a date and validates it
  def dateParser[_: P]: P[String] =
    P(yearParser.! ~ "-" ~ monthParser.! ~ "-" ~ dayParser.!)
      .map { case (year, month, day) => (year.toInt, month.toInt, day.toInt) }
      .filter {
        case (year, month, day) =>
          if (months31.contains(month) && day > 31) false
          else if (months30.contains(month) && day > 30) false
          else if ((year % 4 == 0) && ((year % 100 != 0) || (year % 400 == 0)) && day > 29) false // leap year
          else if (day > 28) false
          true
      }
      .!

  def hourParser[_: P]: P[Unit] =
    P(
      StringIn(
        "00",
        "01",
        "02",
        "03",
        "04",
        "05",
        "06",
        "07",
        "08",
        "09",
        "10",
        "11",
        "12",
        "13",
        "14",
        "15",
        "16",
        "17",
        "18",
        "19",
        "20",
        "21",
        "22",
        "23"
      )
    )
  def minSecondParser[_: P]: P[Unit] =
    P(
      StringIn(
        "00",
        "01",
        "02",
        "03",
        "04",
        "05",
        "06",
        "07",
        "08",
        "09",
        "10",
        "11",
        "12",
        "13",
        "14",
        "15",
        "16",
        "17",
        "18",
        "19",
        "20",
        "21",
        "22",
        "23",
        "24",
        "25",
        "26",
        "27",
        "28",
        "29",
        "30",
        "31",
        "32",
        "33",
        "34",
        "35",
        "36",
        "37",
        "38",
        "39",
        "40",
        "41",
        "42",
        "43",
        "44",
        "45",
        "46",
        "47",
        "48",
        "49",
        "50",
        "51",
        "52",
        "53",
        "54",
        "55",
        "56",
        "57",
        "58",
        "59"
      )
    )

  def milliParser[_: P]: P[Unit]                 = P(digits.rep(min = 3, max = 3))
  def timeParser[_: P]: P[Unit]                  = P(hourParser ~ ":" ~ minSecondParser ~ ":" ~ minSecondParser ~ "." ~ milliParser)
  def dateTimeParser[_: P]: P[String]            = P(dateParser ~ "T" ~ timeParser ~ "Z").!
  def dateTimeLiteral[_: P]: P[DateTimeConstant] = dateTimeParser.map(s => DateTimeConstant(Instant.parse(s)))

}
