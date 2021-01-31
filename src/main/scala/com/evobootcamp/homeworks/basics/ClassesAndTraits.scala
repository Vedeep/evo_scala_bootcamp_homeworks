package com.evobootcamp.homeworks.basics

object ClassesAndTraits {
  def main(args: Array[String]): Unit = {

  }

  sealed trait Shape extends Located with Bounded

  sealed trait Located {
    def x: Double
    def y: Double
  }

  sealed trait Bounded {
    def minX: Double
    def maxX: Double
    def minY: Double
    def maxY: Double
  }

  final case class Point(x: Double, y: Double) extends Shape {
    override def minX: Double = x
    override def maxX: Double = x
    override def minY: Double = y
    override def maxY: Double = y
  }

  // Homework
  //
  // Add additional 2D shapes such as triangle and square.
  //
  // In addition to the 2D shapes classes, add also 3D shapes classes
  // (origin, point, sphere, cube, cuboid, 3D triangle - you can add
  // others if you think they are a good fit).
  //
  // Add method `area` to 2D shapes.
  //
  // Add methods `surfaceArea` and `volume` to 3D shapes.
  //
  // If some of the implementation involves advanced math, it is OK
  // to skip it (leave unimplemented), the primary intent of this
  // exercise is modelling using case classes and traits, and not math.

  sealed trait Area {
    def area: Double
  }

  def getPointsLength(p1: Point, p2: Point): Double = {
    math.sqrt(math.abs(math.pow(p1.x - p2.x, 2) + math.pow(p1.y - p2.y, 2)))
  }

  def getPointsLength(p1: Point3D, p2: Point3D): Double = {
    math.sqrt(math.abs(math.pow(p1.x - p2.x, 2) + math.pow(p1.y - p2.y, 2) + math.pow(p1.z - p2.z, 2)))
  }

  final case class Triangle(p1: Point, p2: Point, p3: Point) extends Shape with Area {
    override def x: Double = (p1.x + p2.x + p3.x) / 3
    override def y: Double = (p1.y + p2.y + p3.y) / 3
    override def minX: Double = List(p1.x, p2.x, p3.x).min
    override def maxX: Double = List(p1.x, p2.x, p3.x).max
    override def minY: Double = List(p1.y, p2.y, p3.y).min
    override def maxY: Double = List(p1.y, p2.y, p3.y).max

    override def area = {
      val l1 = getPointsLength(p1, p2)
      val l2 = getPointsLength(p2, p3)
      val l3 = getPointsLength(p1, p3)

      val p = (l1 + l2 + l3) / 2

      math.sqrt(math.abs(p * (p - l1) * (p - l2) * (p - l3)))
    }
  }

  final case class Square(leftTop: Point, rightBottom: Point) extends Shape with Area {
    override def x: Double = (leftTop.x + rightBottom.x) / 2
    override def y: Double = (leftTop.y + rightBottom.y) / 2
    override def minX: Double = List(leftTop.x, rightBottom.x).min
    override def maxX: Double = List(leftTop.x, rightBottom.x).max
    override def minY: Double = List(leftTop.y, rightBottom.y).min
    override def maxY: Double = List(leftTop.y, rightBottom.y).max

    override def area: Double = {
      math.pow(getPointsLength(leftTop, rightBottom), 2) / 2
    }
  }

  sealed trait Located3D extends Located {
    def z: Double;
  }

  sealed trait Bounded3D extends Bounded {
    def minZ: Double
    def maxZ: Double
  }

  sealed trait Shape3D extends Located3D with Bounded3D

  sealed trait Calculable3D {
    def surfaceArea: Double
    def volume: Double
  }

  final case class Point3D(x: Double, y: Double, z: Double) extends Shape3D {
    override def minX: Double = x
    override def maxX: Double = x
    override def minY: Double = y
    override def maxY: Double = y
    override def minZ: Double = z;
    override def maxZ: Double = z;
  }

  final case class Sphere(centerPoint: Point3D, radius: Double) extends Shape3D with Calculable3D {
    override def x: Double = centerPoint.x
    override def y: Double = centerPoint.y
    override def z: Double = centerPoint.z
    override def minX: Double = x - radius
    override def maxX: Double = x + radius
    override def minY: Double = y - radius
    override def maxY: Double = y + radius
    override def minZ: Double = z - radius
    override def maxZ: Double = z + radius

    override def surfaceArea: Double = 4 * math.Pi * math.pow(radius, 2)
    override def volume: Double = (4 * math.Pi * math.pow(radius, 3)) / 3
  }

  final case class Cube(p1: Point3D, p2: Point3D) extends Shape3D with Calculable3D {
    override def x: Double = (p1.x + p2.x) / 2
    override def y: Double = (p1.y + p2.y) / 2
    override def z: Double = (p1.z + p2.z) / 2
    override def minX: Double = List(p1.x, p2.x).min
    override def maxX: Double = List(p1.x, p2.x).max
    override def minY: Double = List(p1.y, p2.y).min
    override def maxY: Double = List(p1.y, p2.y).max
    override def minZ: Double = List(p1.z, p2.z).min
    override def maxZ: Double = List(p1.z, p2.z).max

    override def surfaceArea: Double = 6 * math.pow(getPointsLength(p1, p2), 2)
    override def volume: Double = math.pow(getPointsLength(p1, p2), 3)
  }
}
