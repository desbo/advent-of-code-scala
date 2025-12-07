package aoc.twentyfive

import aoc.Solution
import aoc.util.{Grid, Vec2}

import scala.collection.mutable

object Day7 extends Solution[Grid[Char], Long]:
  override def parse(input: String): Grid[Char] =
    Grid.chars(input)

  override def part1(input: Grid[Char]): Long =
    val start = input.data.head.find(_._1 == 'S').get._2.x

    input.data
      .foldLeft((Set(start), 0)):
        case ((positions, splits), line) =>
          line.foldLeft((positions, splits)):
            case ((ps, splits), ('^', v)) =>
              val newPositions = (ps - v.x) ++ Set(v.x - 1, v.x + 1)
              if ps.contains(v.x) then (newPositions, splits + 1)
              else (newPositions, splits)

            case ((ps, splits), _) => (ps, splits)
      ._2

  override def part2(input: Grid[Char]): Long =
    import collection.mutable.*

    val dp = mutable.Map.empty[Int, mutable.Map[Int, Long]]
    (0 until input.height).foreach(i => dp(i) = mutable.Map.from((0 until input.width).map(_ -> 0)))
    val start = input.data.head.find(_._1 == 'S').get._2.x
    dp(0)(start) = 1

    input.data
      .foreach: row =>
        row.foreach: (char, vec) =>
          (char, vec) match
            case ('^', v) =>
              dp(v.y)(v.x + 1) += dp(v.y - 1)(v.x)
              dp(v.y)(v.x - 1) += dp(v.y - 1)(v.x)
              dp(v.y)(v.x) = 0 // impossible to reach cell directly under splitter
            case (_, v) =>
              if v.y > 0 then dp(v.y)(v.x) += dp(v.y - 1)(v.x)

    dp(input.height - 1).values.sum

  def render(grid: Grid[Char], dp: mutable.Map[Int, mutable.Map[Int, Long]]): Grid[Char] =
    dp.foldLeft(grid):
      case (g, (y, rows)) =>
        rows.foldLeft(g):
          case (gg, (x, c)) =>
            if c != 0 then gg.update(Vec2(x, y), s"$c".charAt(0))
            else gg
