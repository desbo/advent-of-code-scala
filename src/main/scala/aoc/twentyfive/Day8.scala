package aoc.twentyfive

import aoc.Solution
import aoc.util.Vec3
import collection.mutable

object Day8 extends Solution[Set[Vec3], Long]:
  override def parse(input: String): Set[Vec3] =
    input.linesIterator.toSet.map: s =>
      s.split(",").toList match
        case List(a, b, c) => Vec3(a.toInt, b.toInt, c.toInt)

  case class PointPair(a: Vec3, b: Vec3, distance: Double):
    val set: Set[Vec3] = Set(a, b)

  object PointPair:
    given Ordering[PointPair] with
      override def compare(x: PointPair, y: PointPair): Int =
        y.distance.compare(x.distance)

  def init(input: Set[Vec3]): (mutable.PriorityQueue[PointPair], mutable.ArrayBuffer[mutable.Set[Vec3]]) =
    val seen      = mutable.Set.empty[Set[Vec3]]
    val distances = new mutable.PriorityQueue[PointPair]()

    input.foreach: v =>
      (input - v).foreach: u =>
        if !seen(Set(v, u)) then
          distances.enqueue(PointPair(v, u, v.distance(u)))
          seen += Set(v, u)

    val circuits = mutable.ArrayBuffer.from(seen.flatten.map(mutable.Set(_)))

    (distances, circuits)

  def connect(pair: PointPair, circuits: mutable.ArrayBuffer[mutable.Set[Vec3]]): Unit =
    val i = circuits.indexWhere(_.contains(pair.a))
    val j = circuits.indexWhere(_.contains(pair.b))
    if i != j && i >= 0 && j >= 0 then
      circuits(i) ++= circuits(j)
      circuits.remove(j)

  override def part1(input: Set[Vec3]): Long =
    val (distances, circuits) = init(input)
    val size                  = if input.size > 100 then 1000 else 10

    for i <- 0 until size do
      val pair = distances.dequeue
      connect(pair, circuits)

    circuits.map(_.size).sorted.reverse.take(3).product

  override def part2(input: Set[Vec3]): Long =
    val (distances, circuits) = init(input)
    var pair: PointPair       = null

    while circuits.size > 1 do
      pair = distances.dequeue
      connect(pair, circuits)

    pair.a.x * pair.b.x
