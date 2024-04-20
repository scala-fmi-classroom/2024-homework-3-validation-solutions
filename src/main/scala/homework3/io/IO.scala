package homework3.io

import scala.annotation.tailrec
import scala.io.StdIn

sealed trait IO[+A]:
  def map[B](f: A => B): IO[B] = flatMap(a => IO.of(f(a)))
  def flatMap[B](f: A => IO[B]): IO[B] = FlatMap(this, f)

  def >>=[B](f: A => IO[B]): IO[B] = flatMap(f)
  def >>[B](nextIO: IO[B]): IO[B] = flatMap(_ => nextIO)

  @tailrec
  final def unsafeRun(): A = this match
    case Pure(value) => value
    case Delay(delayedValue) => delayedValue()
    case FlatMap(Pure(value), f) => f(value).unsafeRun()
    case FlatMap(Delay(delayedValue), f) => f(delayedValue()).unsafeRun()
    case FlatMap(FlatMap(ioInner, f), g) => ioInner.flatMap(value => f(value).flatMap(g)).unsafeRun()

case class Pure[A](value: A) extends IO[A]
case class Delay[A](delayedValue: () => A) extends IO[A]
case class FlatMap[A, B](ioA: IO[A], f: A => IO[B]) extends IO[B]

object IO:
  def apply[A](delayedValue: => A): IO[A] = Delay(() => delayedValue)
  def of[A](value: A): IO[A] = Pure(value)

  def println(str: String): IO[Unit] = IO(Predef.println(str))
  def readln: IO[String] = IO(StdIn.readLine())
