package aoc
package util

import cats.syntax.all.*
import cats.{Eval, Foldable, Functor, Show}

import scala.collection.immutable.ListMap

case class Grid[A](data: Vector[Vector[(A, Vec2)]]):
  lazy val points: ListMap[Vec2, A] =
    data.foldLeft(ListMap.empty[Vec2, A]):
      case (points, as) =>
        points ++ as.foldLeft(points):
          case (points, (a, point)) => points.updated(point, a)

  lazy val positions: Map[A, Set[Vec2]] =
    points.groupBy(_._2).view.mapValues(_.keySet).toMap

  def at(pos: Vec2): Option[A] = points.get(pos)

  def update(pos: Vec2, a: A): Grid[A] =
    Grid:
      data.updated(pos.y, data(pos.y).updated(pos.x, (a, pos)))

  def swap(a: Vec2, b: Vec2): Grid[A] =
    update(a, points(b)).update(b, points(a))

  def covers(point: Vec2): Boolean =
    points.keySet.contains(point)

  def positionalMap[B](f: (A, Vec2) => B): Grid[B] = Grid:
    data.map(_.map((a, v) => f(a, v) -> v))

  def render(using Show[A]): String =
    data
      .map: vec =>
        vec._1F.map(_.show).mkString
      .mkString("\n")

  def positionalFoldLeft[B](b: B)(f: (B, (Vec2, A)) => B): B =
    data.foldl(b): (b, vec) =>
      vec.foldl(b):
        case (b, (a, xy)) => f(b, (xy, a))

  val width: Int  = data.headOption.map(_.size).getOrElse(0)
  val height: Int = data.size

object Grid:
  def chars(input: String): Grid[Char] =
    Grid:
      input.linesIterator.zipWithIndex.toVector.map:
        case (str, y) =>
          str.zipWithIndex.toVector.map:
            case (chr, x) => (chr, Vec2(x, y))

  given Functor[Grid] with
    override def map[A, B](fa: Grid[A])(f: A => B): Grid[B] =
      Grid(fa.data.map(_.map((a, v) => (f(a), v))))

  given Foldable[Grid] with
    override def foldLeft[A, B](fa: Grid[A], b: B)(f: (B, A) => B): B =
      fa.data.foldl(b): (b, vec) =>
        vec.foldl(b):
          case (b, (a, _)) => f(b, a)

    override def foldRight[A, B](fa: Grid[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      fa.data.foldr(lb): (vec, b) =>
        vec.foldr(b):
          case ((a, _), b) => f(a, b)
