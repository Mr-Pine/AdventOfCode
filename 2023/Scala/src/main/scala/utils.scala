package de.mr_pine.aoc.y2023

def getInput(filename: String): String = {
  val source = io.Source.fromFile(s"./data/$filename")
  try source.getLines() mkString "\n" finally source.close()
}

def getInput(day: Int, example: Boolean): String = getInput(s"$day.${if (example) "example" else "input"}")

