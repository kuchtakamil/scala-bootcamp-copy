package com.evolutiongaming.bootcamp.typeclass.v3_typeclass

import cats.Show
import cats.syntax.show
import com.evolutiongaming.bootcamp.basics.ControlStructures.c
import com.evolutiongaming.bootcamp.typeclass.v3_typeclass.FPJson.Jsonable

/**
  * Try to accomplish as many tasks as you can
  */
object TypeClassesHomework {

  object OrderingTask {

    final case class Money(amount: BigDecimal)

    implicit val moneyOrdering: Ordering[Money] = Ordering.by(_.amount)
  }

  object ShowTask {

    trait Show[T] { // Fancy toString
      def show(entity: T): String
    }

    final case class User(id: String, name: String)

    implicit val showUser: Show[User] = new Show[User] {
      override def show(entity: User): String = s"${entity.id.toString}${entity.name.toString}"
    }

    def prettyShowUser[A](a: A)(implicit showMachine: Show[A]): String = showMachine.show(a)

    prettyShowUser(User("su", "Master"))


    object ShowSyntax {
      implicit class ShowOps[A](a: A){
        def show(implicit showMachine: Show[A]): String = showMachine.show(a)
      }
    }
    import ShowSyntax._
    User("su", "Disaster").show
  }

  object ParseTask {
    case class Csv(content: String) {
      def get: String = content
    }
    type Error = String

    trait Parse[T] { // Feel free to use any format. It could be CSV or anything else.
      def parse(entity: Csv): Either[Error, T]
    }

    final case class User(id: String, name: String)

    implicit val parseUser: Parse[User] = new Parse[User] {
      override def parse(entity: Csv): Either[Error, User] = {
        val strings = entity.get.split(",")
        if (strings.length == 2) Right(User(strings(0), strings(1)))
        else Left("Invalid input")
      }
    }

    def printParsed[A](a: Csv)(implicit parser: Parse[A]): Either[Error, A] = parser.parse(a)

    object ParseSyntax {
      implicit class ParseOps(csv: Csv) {
        def parse[A](implicit parser: Parse[A]): Either[Error, A] = parser.parse(csv)
      }
    }
    import ParseSyntax._
    Csv("invalid").parse // => Invalid input
    Csv("Pies,Reksio").parse

  }

  object EqualsTask {
    // TODO Design a typesafe equals so I can do a === b, but it won't compile if a and b are of different types
    // Define the typeclass (think of a method signature)
    // Keep in mind that `a method b` is `a.method(b)`

    trait TypeEquals[A] {
      def typeEquals(a: A, b: A): Boolean
    }

      implicit def typeEqualsInstance[A]: TypeEquals[A] = new TypeEquals[A] {
        def typeEquals(a: A, b: A): Boolean = a == b
      }
//      import TypeEquals._
      implicit class TypeSafeEquals[A](a: A) {
        def ===(b: A)(implicit ev: TypeEquals[A]): Boolean = ev.typeEquals(a, b)
      }
      val x: Int = 1
      val y: Int = 2
      val z: String = "hello"

      x === y // true
      x === z
  }

  object Foldable {

    trait Semigroup[A] {
      def combine(x: A, y: A): A
    }

    trait Monoid[A] extends Semigroup[A] {
      def empty: A
    }

    trait Foldable[F[_]] {
      def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B
      def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B
      def foldMap[A, B](as: F[A])(f: A => B)(implicit monoid: Monoid[B]): B
    }

    implicit val optionFoldable: Foldable[Option] = new Foldable[Option] {
      override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B =
        as match {
          case Some(a) => f(z, a)
          case None => z
        }

      override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B = as match {
        case Some(a) => f(a, z)
        case None => z
      }

      override def foldMap[A, B](as: Option[A])(f: A => B)(implicit monoid: Monoid[B]): B = as match {
        case Some(a) => f(a)
        case None => monoid.empty
      }
    }  // TODO Implement Foldable instance for Option

    implicit val listFoldable: Foldable[List] = new Foldable[List] {
      override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
        as.foldLeft(z)(f)

      override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
        as.foldRight(z)(f)

      override def foldMap[A, B](as: List[A])(f: A => B)(implicit monoid: Monoid[B]): B =
        as.foldLeft(monoid.empty)((acc, a) => monoid.combine(acc, f(a)))
    } // TODO Implement Foldable instance for List

    sealed trait Tree[A]
    object Tree {
      final case class Leaf[A](value: A) extends Tree[A]
      final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
    }

    implicit val treeFoldable: Foldable[Tree] = new Foldable[Tree] {
      override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = ???

      override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = ???

      override def foldMap[A, B](as: Tree[A])(f: A => B)(implicit monoid: Monoid[B]): B = ???
    } // TODO Implement Foldable instance for Tree
  }

  object ApplicativeTask {

    trait Semigroupal[F[_]] {
      def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
    }

    trait Functor[F[_]] {
      def map[A, B](fa: F[A])(f: A => B): F[B]
    }

    trait Apply[F[_]] extends Functor[F] with Semigroupal[F] {

      def ap[A, B](fab: F[A => B])(fa: F[A]): F[B] // "ap" here stands for "apply" but it's better to avoid using it

      override def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = ??? // TODO Implement using `ap` and `map`

      def map2[A, B, Z](fa: F[A], fb: F[B])(f: (A, B) => Z): F[Z] = ??? // TODO Implement using `map` and `product`
    }

    trait Applicative[F[_]] extends Apply[F] {
      def pure[A](a: A): F[A]
    }

    // TODO Implement Applicative instantce for Option
    implicit val optionApplicative: Applicative[Option] = ??? // Keep in mind that Option has flatMap

    // TODO Implement traverse using `map2`
    def traverse[F[_]: Applicative, A, B](as: List[A])(f: A => F[B]): F[List[B]] = ???

    // TODO Implement sequence (ideally using already defined things)
    def sequence[F[_]: Applicative, A](fas: List[F[A]]): F[List[A]] =
      traverse(fas)(fa => fa)
  }
}
