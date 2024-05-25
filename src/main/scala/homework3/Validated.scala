package homework3

sealed trait Validated[+E, +A]:
  def isValid: Boolean = fold(_ => false, _ => true)

  def getOrElse[B >: A](default: => B): B = fold(_ => default, identity)

  def orElse[F >: E, B >: A](default: => Validated[F, B]): Validated[F, B] =
    fold(_ => default, _ => this)

  infix def zip[EE >: E, B](vb: Validated[EE, B]): Validated[EE, (A, B)] = (this, vb) match
    case (Valid(a), Valid(b)) => Valid((a, b))
    case (Invalid(errors1), Invalid(errors2)) => Invalid(errors1 ++ errors2)
    case (i @ Invalid(_), _) => i
    case (_, i @ Invalid(_)) => i

  def map[B](f: A => B): Validated[E, B] = flatMap(a => Valid(f(a)))

  def mapErrors[EE](f: Chain[E] => EE): Validated[EE, A] =
    fold(errors => Invalid(f(errors)), a => Valid(a))

  def zipMap[EE >: E, B, R](vb: Validated[EE, B])(f: (A, B) => R): Validated[EE, R] =
    (this zip vb).map(f.tupled)

  def flatMap[EE >: E, B](f: A => Validated[EE, B]): Validated[EE, B] =
    fold(errors => Invalid(errors), f)

  def fold[B](fInvalid: Chain[E] => B, f: A => B): B =
    this match
      case Valid(a) => f(a)
      case Invalid(errors) => fInvalid(errors)

  def foreach(f: A => Unit): Unit = fold(_ => (), f)

case class Valid[+A](value: A) extends Validated[Nothing, A]
case class Invalid[+E](errors: Chain[E]) extends Validated[E, Nothing]

object Invalid:
  def apply[E](error: E): Invalid[E] = Invalid(Chain(error))

object Validated:
  extension [EE, A, B](
    tuple: (
      Validated[EE, A],
      Validated[EE, B]
    )
  )
    def zipN: Validated[EE, (A, B)] =
      val (va, vb) = tuple
      va zip vb
    def mapN[R](f: (A, B) => R): Validated[EE, R] = tuple.zipN.map(f.tupled)

  extension [EE, A, B, C](
    tuple: (
      Validated[EE, A],
      Validated[EE, B],
      Validated[EE, C]
    )
  )
    def zipN: Validated[EE, (A, B, C)] =
      tuple.head.zip(tuple.tail.zipN).map(tuple => tuple.head *: tuple.last)

    def mapN[R](f: (A, B, C) => R): Validated[EE, R] = tuple.zipN.map(f.tupled)

  extension [EE, A, B, C, D](
    tuple: (
      Validated[EE, A],
      Validated[EE, B],
      Validated[EE, C],
      Validated[EE, D]
    )
  )
    def zipN: Validated[EE, (A, B, C, D)] =
      tuple.head.zip(tuple.tail.zipN).map(tuple => tuple.head *: tuple.last)
    def mapN[R](f: (A, B, C, D) => R): Validated[EE, R] = tuple.zipN.map(f.tupled)

  extension [EE, A, B, C, D, E](
    tuple: (
      Validated[EE, A],
      Validated[EE, B],
      Validated[EE, C],
      Validated[EE, D],
      Validated[EE, E]
    )
  )
    def zipN: Validated[EE, (A, B, C, D, E)] =
      tuple.head.zip(tuple.tail.zipN).map(tuple => tuple.head *: tuple.last)
    def mapN[R](f: (A, B, C, D, E) => R): Validated[EE, R] = tuple.zipN.map(f.tupled)

  def sequence[E, A](xs: List[Validated[E, A]]): Validated[E, List[A]] =
    val initial: Validated[E, List[A]] = Valid(List.empty)
    xs.foldRight(initial)((next, acc) => next.zipMap(acc)(_ :: _))

  def onCondition[E, A](value: A, condition: A => Boolean, error: E): Validated[E, A] =
    if condition(value) then Valid(value)
    else Invalid(error)

  extension [A](opt: Option[A])
    def toValidated[E](onEmpty: => E): Validated[E, A] =
      opt.fold(Invalid(onEmpty))(Valid(_))
