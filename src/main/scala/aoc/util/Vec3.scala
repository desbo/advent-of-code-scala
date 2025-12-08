package aoc.util

case class Vec3(x: Int, y: Int, z: Int):
  override def toString: String = s"[$x $y $z]"

  def -(other: Vec3): Vec3 =
    Vec3(x - other.x, y - other.y, z - other.z)

  def +(other: Vec3): Vec3 =
    Vec3(x + other.x, y + other.y, z + other.z)

  def distance(other: Vec3): Double =
    math.sqrt(
      math.pow(x - other.x, 2) +
        math.pow(y - other.y, 2) +
        math.pow(z - other.z, 2)
    )
