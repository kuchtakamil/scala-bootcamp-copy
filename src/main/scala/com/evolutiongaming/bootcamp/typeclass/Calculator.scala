package com.evolutiongaming.bootcamp.typeclass

object Calculator extends App {

  object Numeric {
    // type class
    trait Numeric[T] {
      def add(a: T, b: T): T

      def mul(a: T, b: T): T

      def square(a: T): T = mul(a, a)
    }

    object ops {
      implicit class NumericOps[T](a: T)(implicit numeric: Numeric[T]) {
        def add(b: T): T = numeric.add(a, b)
        def mul(b: T): T = numeric.mul(a, b)
      }
    }

    // instance of type class
    implicit val intNumeric: Numeric[Int] = new Numeric[Int] {
      override def add(a: Int, b: Int): Int = a + b
      override def mul(a: Int, b: Int): Int = a * b
    }

    implicit val stringNumeric: Numeric[String] = new Numeric[String] {
      override def add(a: String, b: String): String = a + b
      override def mul(a: String, b: String): String = for {
        as <- a
        bs <- b
        s  <- as.toString ++ bs.toString
      } yield s
    }
  }

  import Numeric._
  import Numeric.ops._

  def sumList[T](ts: List[T])(implicit numeric: Numeric[T]): T = {
//    ts.reduce((a, b) => numeric.add(a, b)) // option 1
    ts.reduce((a, b) => a.add(a))
  }

  {
    sumList(List(1, 2, 3))
    println(stringNumeric.mul("abc", "def"))
  }

  {
    val s1 = "gt"
    val s2 = "rs"
    s1.mul(s2)
  }
}
