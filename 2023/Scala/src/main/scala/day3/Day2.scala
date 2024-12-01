package de.mr_pine.aoc.y2023
package day3

import java.util
import scala.annotation.tailrec

object Day3 extends Day[Array[Game]] {
  override def init(example: Boolean): Array[Game] = getInput(2, example).split("\n").map(Game.apply)

  override def part1(example: Boolean): String = init(example).filter(_.subsets.forall(set => set.red <= 12 && set.green <= 13 && set.blue <= 14)).map(_.id).sum.toString

  override def part2(example: Boolean): String = init(example).map(_.power).sum.toString
}

private case class Engine(numbers: List[Int], Board: Schematic)

private object Engine {
  def apply(string: String): Engine = {
    val numbers = List()

  }
}

private type SchematicValue = EnginePart | NumberReference | Unit

private case class Schematic(representation: Array[Array[SchematicValue]])

private case class EnginePart()

private case class NumberReference(value: Int, hit: Boolean = false)