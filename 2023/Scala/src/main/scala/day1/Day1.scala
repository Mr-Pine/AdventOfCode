package de.mr_pine.aoc.y2023
package day1

object Day1 extends Day[Array[CalibrationLine]] {
  override def init(example: Boolean): Array[CalibrationLine] = {
    val fun = getInput(1, example).split("\n").map(CalibrationLine.apply)
    fun
  }

  override def part1(example: Boolean): String = init(example).map(_.value).sum.toString

  override def part2(example: Boolean): String = init(example).map(_.extractTextNumbers).map(_.value).sum.toString
}


private case class CalibrationLine(text: String) {
  def extractTextNumbers: CalibrationLine = CalibrationLine(
    text
      .replace("one", "one1one")
      .replace("two", "two2two")
      .replace("three", "three3three")
      .replace("four", "four4four")
      .replace("five", "five5five")
      .replace("six", "six6six")
      .replace("seven", "seven7seven")
      .replace("eight", "eight8eight")
      .replace("nine", "nine9nine")
  )

  def value: Int = {
    val digits = text.map {
      case x if '0' <= x && x <= '9' => x.toString
      case _ => ""
    } mkString ""
    val number = digits.take(1) + digits.last
    Integer.parseInt(number)
  }
}