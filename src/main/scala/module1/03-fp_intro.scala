package module1

import java.util.UUID

import scala.annotation.tailrec
import java.time.Instant

import module1.list.List.Cons


/**
 * referential transparency
 */
object referential_transparency {


  case class Abiturient(id: String, email: String, fio: String)

  type Html = String

  sealed trait Notification

  object Notification {

    case class Email(email: String, text: Html) extends Notification

    case class Sms(telephone: String, msg: String) extends Notification

  }


  case class AbiturientDTO(email: String, fio: String, password: String)

  trait NotificationService {
    def sendNotification(notification: Notification): Unit

    def createNotification(abiturient: Abiturient): Notification
  }

  trait AbiturientService {

    def registerAbiturient(abiturientDTO: AbiturientDTO): Abiturient
  }

  class AbiturientServiceImpl(notificationService: NotificationService) extends AbiturientService {

    override def registerAbiturient(abiturientDTO: AbiturientDTO): Abiturient = {
      val abiturient = Abiturient(UUID.randomUUID().toString(), abiturientDTO.email, abiturientDTO.fio)
      notificationService.sendNotification(Notification.Email(abiturient.email, "Some message"))
      abiturient
    }

    def registerAbiturient2(uuid: UUID, abiturientDTO: AbiturientDTO): Abiturient = {
      val abiturient = Abiturient(uuid.toString(), abiturientDTO.email, abiturientDTO.fio)
      abiturient
    }

  }

}


// recursion

object recursion {

  /**
   * Реализовать метод вычисления n!
   * n! = 1 * 2 * ... n
   */

  def fact(n: Int): Int = {
    var _n = 1
    var i = 2
    while (i <= n) {
      _n *= i
      i += 1
    }
    _n
  }

  def factRec(n: Int): Int =
    if (n == 1) 1
    else n * factRec(n - 1)

  def factTailRec(n: Int): Int = {
    @tailrec
    def loop(n: Int, accum: Int): Int =
      if (n == 1) accum
      else loop(n - 1, n * accum)

    loop(n, 1)
  }


  /**
   * реализовать вычисление N числа Фибоначчи
   * F0 = 0, F1 = 1, Fn = Fn-1 + Fn - 2
   *
   */

  def fib(n: Int): Int = ???

}

object hof {

  def printFactorialResult(r: Int) = println(s"Factorial result is ${r}")

  def printFibonacciResult(r: Int) = println(s"Fibonacci result is ${r}")

  def printResult[T](r: T, funcName: String) = println(s"$funcName result is ${r}")

  def printRunningTimeFunc1[A, B](a: A)(f: A => B): Unit = {
    val current = Instant.now().toEpochMilli()
    f(a)
    val current2 = Instant.now().toEpochMilli()
    println(current2 - current)
  }


  // Follow type implementation
  def partial[A, B, C](a: A, f: (A, B) => C): B => C = b => f(a, b)

  def sum(x: Int, y: Int): Int = x + y

  val r: Int => Int = partial(1, sum)

}


/**
 * Реализуем тип Option
 */


object opt {

  /**
   *
   * Реализовать тип Option, который будет указывать на присутствие либо отсутсвие результата
   */

  // Animal
  // Dog extend Animal
  // Option[Dog] Option[Animal]

  sealed trait Option[+T] {
    def isEmpty: Boolean = this match {
      case Option.Some(v) => false
      case Option.None => true
    }

    def get: T = this match {
      case Option.Some(v) => v
      case Option.None => throw new Exception("Get on empty Option")
    }

    def getOrElse[TT >: T](b: TT): TT = this match {
      case Option.Some(v) => v
      case Option.None => b
    }

    def map[B](f: T => B): Option[B] = this match {
      case Option.Some(v) => Option.Some(f(v))
      case Option.None => Option.None
    }

    def flatMap[B](f: T => Option[B]): Option[B] = this match {
      case Option.Some(v) => f(v)
      case Option.None => Option.None
    }

    def printIfAny: Unit = this match {
      case Option.Some(v) => println(s"Value of the option is $v")
      case Option.None =>
    }

    def filter(predicate: T => Boolean): Option[T] = this match {
      case base@Option.Some(v) => if (predicate(v)) base else Option.None
      case _ => Option.None
    }

    def zip[T2]( o2: Option[T2]): Option[(T, T2)] = (this, o2) match {
      case (Option.Some(v1), Option.Some(v2)) => Option.Some((v1, v2))
      case _ => Option.None
    }
  }

  object Option {

    case class Some[T](v: T) extends Option[T]

    case object None extends Option[Nothing]

  }


  /**
   *
   * Реализовать метод printIfAny, который будет печатать значение, если оно есть
   */




  /**
   *
   * Реализовать метод zip, который будет создавать Option от пары значений из 2-х Option
   */




  /**
   *
   * Реализовать метод filter, который будет возвращать не пустой Option
   * в случае если исходный не пуст и предикат от значения = true
   */



}

object list {

  sealed trait List[+T] {
    def ::[TT >: T](el: TT): List[TT] = List.Cons(el, this)

    def mkString(sep: String): String = {
      @tailrec
      def loop(lst: List[T], acc: String): String = lst match {
        case List.Nil => acc
        case List.Cons(h, t) => loop(t, acc + sep + h)
      }

      loop(this, "")
    }

    def reverse = {
      @tailrec
      def loop(acc: List[T], lstToTraverse: List[T]): List[T] = lstToTraverse match {
        case List.Cons(head, tail) => loop(Cons(head, acc), tail)
        case List.Nil => acc
      }

      loop(List.Nil, this)
    }

    def map[B](f: T => B): List[B] = {
      @tailrec
      def loop(acc: List[B], lstToTraverse: List[T]): List[B] = lstToTraverse match {
        case Cons(head, tail) => loop(Cons(f(head), acc), tail)
        case List.Nil => acc
      }

      loop(List.Nil, this).reverse
    }

    def flatMap[B]( f : T => List[B]) : List[B] = {
      @tailrec
      def loop(acc : List[B], lstToTraverse : List[T]) : List[B] = lstToTraverse match {
        case Cons(head, tail) => loop(mergeIntoHead(acc, f(head)), tail)
        case List.Nil => acc
      }
      @tailrec
      def mergeIntoHead(acc : List[B], lstToMerge : List[B]) : List[B] = lstToMerge match {
        case Cons(head, tail) => mergeIntoHead(Cons(head, acc), tail)
        case List.Nil => acc
      }
      loop(List.Nil, this)
    }

    def filter(predicate : T => Boolean) : List[T] = {
      @tailrec
      def loop(acc : List[T], lstToTraverse : List[T]) : List[T] = lstToTraverse match {
        case Cons(head, tail) => if(predicate(head)) loop(Cons(head, acc), tail) else loop(acc, tail)
        case List.Nil => acc
      }

      loop(List.Nil, this).reverse
    }


  }

  object List {

    case class Cons[T](head: T, tail: List[T]) extends List[T]

    case object Nil extends List[Nothing]

    def apply[T](elems: T*): List[T] = {
      @tailrec
      def loop(acc: List[T], elemsSeq: T*): List[T] = {
        elemsSeq.headOption match {
          case Some(value) => loop(Cons(value, acc), elemsSeq.tail: _*)
          case None => acc
        }
      }

      loop(List.Nil, elems: _*)
    }

    //OR
    def apply2[T](elems: T*): List[T] = elems.foldLeft[List[T]](Nil)((acc, el) => Cons(el, acc))

    def incList(lst : List[Int]) = lst.map( _ + 1)

    def shoutString(lst : List[String]) = lst.map(el => "!" + el)
  }

  /**
   *
   * Реализовать односвязанный иммутабельный список List
   * Список имеет два случая:
   * Nil - пустой список
   * Cons - непустой, содердит первый элемент (голову) и хвост (оставшийся список)
   */


  /**
   * Метод cons, добавляет элемент в голову списка, для этого метода можно воспользоваться названием `::`
   *
   */


  /**
   * Метод mkString возвращает строковое представление списка, с учетом переданного разделителя
   *
   */

  /**
   * Конструктор, позволяющий создать список из N - го числа аргументов
   * Для этого можно воспользоваться *
   *
   * Например вот этот метод принимает некую последовательность аргументов с типом Int и выводит их на печать
   * def printArgs(args: Int*) = args.foreach(println(_))
   */

  /**
   *
   * Реализовать метод reverse который позволит заменить порядок элементов в списке на противоположный
   */

  /**
   *
   * Реализовать метод map для списка который будет применять некую ф-цию к элементам данного списка
   */


  /**
   *
   * Реализовать метод filter для списка который будет фильтровать список по некому условию
   */

  /**
   *
   * Написать функцию incList котрая будет принимать список Int и возвращать список,
   * где каждый элемент будет увеличен на 1
   */


  /**
   *
   * Написать функцию shoutString котрая будет принимать список String и возвращать список,
   * где к каждому элементу будет добавлен префикс в виде '!'
   */

}