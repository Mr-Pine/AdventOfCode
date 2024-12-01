package de.mr_pine.aoc.y2023
package day2

import scala.annotation.tailrec

object Day2 extends Day[Array[Game]] {
  override def init(example: Boolean): Array[Game] = getInput(2, example).split("\n").map(Game.apply)

  override def part1(example: Boolean): String = init(example).filter(_.subsets.forall(set => set.red <= 12 && set.green <= 13 && set.blue <= 14)).map(_.id).sum.toString

  override def part2(example: Boolean): String = init(example).map(_.power).sum.toString
}

private case class Game(id: Int, subsets: Array[Subset]) {
  def power: Int = subsets.map(_.red).max * subsets.map(_.green).max * subsets.map(_.blue).max
}

private object Game {
  def apply(line: String): Game = {
    val id = line.split(":")(0).split(" ")(1).toInt
    val subsets = line.split(":")(1).split(";").map(Subset.apply)
    Game(id, subsets)
  }
}

private case class Subset(red: Int, green: Int, blue: Int)

private object Subset {
  private val regexBlue = "(?<amount>\\d+) blue".r
  private val regexRed = "(?<amount>\\d+) red".r
  private val regexGreen = "(?<amount>\\d+) green".r

  def apply(subsetString: String): Subset = {
    val red = regexRed.findFirstMatchIn(subsetString) match {
      case Some(value) => value.group("amount").toInt
      case None => 0
    }

    val blue = regexBlue.findFirstMatchIn(subsetString) match {
      case Some(value) => value.group("amount").toInt
      case None => 0
    }
    val green = regexGreen.findFirstMatchIn(subsetString) match {
      case Some(value) => value.group("amount").toInt
      case None => 0
    }

    Subset(red, green, blue)
  }
}