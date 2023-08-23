package com.evolutiongaming.bootcamp.basics



object ClassesAndTraits {
  // You can follow your progress using the tests in `ClassesAndTraitsSpec`:
  //   sbt "testOnly com.evolutiongaming.bootcamp.basics.ClassesAndTraitsSpec"

  // Classes in Scala are blueprints for creating object instances. They can contain methods, values,
  // variables, types, objects, traits, and classes which are collectively called members.

  class MutableUser(var login: String, var balance: Double) {
    def addMoney(amount: Double): Unit = changeBalance(amount)
    def takeMoney(amount: Double): Unit = changeBalance(-amount)

    private def changeBalance(amount: Double): Unit = {
      balance += amount
    }

    override def toString: String =
      s"User($login, $balance)"
  }

  // Singleton objects are defined using `object`.
  // It is a class that has exactly one instance.
  // An object with the same name as a class is called a companion object.
  //
  // Use it to contain methods and values related to this trait or class, but that aren't
  // specific to instances of this trait or class.
  //
  // User specific methods `apply` and `unapply` to construct and deconstruct classes
  object MutableUser {
    def apply(login: String): MutableUser = new MutableUser(login, 0)

    def unapply(user: MutableUser): Option[(String, Double)] = Some(user.login, user.balance)

    def foo() : Int = 3
    def bar() : Int = 5
  }

  val mutableUser = MutableUser("potter")
  MutableUser.foo()
  mutableUser.login // "potter"
  mutableUser.balance // 0.0

  class MyUser {
    def introduceYourself: String = "I'm not you"
    private def myFun: String = "foo"
    private val myVal: String = "val"
  }
//  class MyBadUser extends MyUser {
//    def destroy = introduceYourself + " and will destroy you"
//  }
  object MyUser {
    def andSaySomething: String = "sheep"  // + introduceYourself
  private val prr: Int = 8
  }
//  object Walter extends MyUser {
//    def sayMyName: String = "Heisenberg" + introduceYourself // + andSaySomething
//  }

//  val myUser = new MyUser
//  val myUser2 = MyUser
//  val walter = Walter
//  myUser.introduceYourself
//  myUser2.andSaySomething
//  walter.sayMyName

  case class Name private (value: String)

  object Name {
    def apply(value: String): Name = new Name(value)
  }

  class Name2 private (val value: String)

  object Name2 {
    def apply(value: String): Name2 = new Name2(value)
  }

  // Question. Is MutableUser a good design? Why or why not?

  // Traits define a common interface that classes conform to. They are similar to Java's interfaces.
  // A trait can be thought of as a contract that defines the capabilities and behaviour of a component.

  // Subtyping
  // Where a given trait is required, a subtype of the trait can be used instead.

  // Classes and singleton objects can extend traits.
  //
  // This allows "programming to the interface" approach where you depend on traits instead of their
  // specific implementations (classes or objects).
  //
  // This makes code more reusable and testable.

  trait HasBalance {
    def balance: Double
  }

  // Exercise. Implement a method that will allow aggregating all entities with balances into one
  // that will have the sum of all balances inside.
  //
  def totalBalance(accounts: List[HasBalance]): HasBalance =
    new HasBalance {
      def balance: Double = accounts.map(_.balance).sum
    }

  trait Account extends HasBalance {
    // def addMoney(amount: Double)
    // def takeMoney(amount: Double)
  }

  sealed trait User {
    def login: String
  }

  final case class RegularUser(login: String, balance: Double) extends User with Account {

  }

  case object Admin extends User {
    val login: String = "admin"
  }

  object Admin2 extends User {
    val login: String = "admin"
  }

  // Case Classes
  //
  // Case classes are like regular classes, but with extra features which make them good for modelling
  // immutable data. They have all the functionality of regular classes, but the compiler generates additional
  // code, such as:
  // - Case class constructor parameters are public `val` fields, publicly accessible
  // - `apply` method is created in the companion object, so you don't need to use `new` to create a new
  //   instance of the class
  // - `unapply` method which allows you to use case classes in `match` expressions (pattern matching)
  // - a `copy` method is generated
  // - `equals` and `hashCode` methods are generated, which let you compare objects & use them in collections
  // - `toString` method is created for easier debugging purposes
  // - extends Serializable
  //
  // Case object provides default equals, hashCode, unapply, toString methods, extends Serializable
  //
  // When declaring a case class, make it final. Otherwise someone may decide to inherit from it
  // case-to-case inheritance is prohibited

  // calls .apply method
  val user = RegularUser("potter", 200)


  // calls .unapply method
  val RegularUser(login, balance) = user

  val updatedUser = user.copy(balance = 1000)

  println(updatedUser.toString) // RegularUser(potter,1000)

  // Let's go back to the `Account` trait, which contains two methods for changing the balance:
  //
  // def addMoney(amount: Double)
  // def takeMoney(amount: Double)
  //
  // How should we implement these methods for all heirs at once?
  // What should be the return type of these methods in `Account`? In `RegularUser`?

  // Generic classes and type parameters

  // In a similar way as we saw with polymorphic methods, classes and traits can also take type parameters.
  // For example, you can define a `Stack[A]` which works with any type of element `A`.

  // Question. Do you agree with how the stack is modelled here? What would you do differently?
  final case class Stack[A](elements: List[A] = Nil) {
    def push(x: A): Stack[A] = ???
    def peek: A              = ???
    def pop: (A, Stack[A])   = ???
  }
}
