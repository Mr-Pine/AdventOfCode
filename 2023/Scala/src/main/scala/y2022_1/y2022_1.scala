package de.mr_pine.aoc.y2023
package y2022_1

class y2022_1 extends Day[Array[Elf]] {
  override def init(example: Boolean): Array[Elf] = getInput("2022_01.input").split("\n\n").map(Elf.apply)

  override def part1(example: Boolean = false): String = {
    val elves = init(false)
    val values = elves.map(_.calories)
    return values.max.toString
  }

  override def part2(example: Boolean = false): String = {
    init(false).map(_.calories).sorted.takeRight(3).sum.toString
  }
}


private case class Elf(items: Array[Int]) {
  def calories: Int = items.sum
}

private object Elf {
  def apply(input: String): Elf = Elf(input.split("\n").map(Integer.parseInt))
}