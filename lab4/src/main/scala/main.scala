sealed trait RegEx[+T]

case object EmptySet extends RegEx[Nothing]
case object Epsilon extends RegEx[Nothing]
case class Symbol[T](value: T) extends RegEx[T]
case class Union[T](left: RegEx[T], right: RegEx[T]) extends RegEx[T]
case class Concat[T](left: RegEx[T], right: RegEx[T]) extends RegEx[T]
case class Star[T](expr: RegEx[T]) extends RegEx[T]

object Main {
  def simplify[T](regex: RegEx[T]): RegEx[T] = regex match {

    case Concat(r1, r2) =>
      val left = simplify(r1)
      val right = simplify(r2)
      (left, right) match {
        case (EmptySet, _) => EmptySet
        case (_, EmptySet) => EmptySet
        case (Epsilon, r) => r
        case (r, Epsilon) => r
        case _ => Concat(left, right)
      }
    case Union(r1, r2) =>
      val left = simplify(r1)
      val right = simplify(r2)
      (left, right) match {
        case (EmptySet, r) => r
        case (r, EmptySet) => r
        case _ => Union(left, right)
      }
    case Star(r) =>
      simplify(r) match {
        case EmptySet => Epsilon
        case Epsilon => Epsilon
        case simplified => Star(simplified)
      }
    case _ => regex
  }

  def main(args: Array[String]): Unit = {
    val regex1: RegEx[Char] = Concat(Symbol('a'), Epsilon)
    val regex2: RegEx[Char] = Union(Symbol('a'), EmptySet)
    val regex3: RegEx[Char] = Star(Epsilon)
    val regex4: RegEx[Char] = Concat(EmptySet, Symbol('a'))

    println(s"Simplify regex1: ${simplify(regex1)}")
    println(s"Simplify regex2: ${simplify(regex2)}")
    println(s"Simplify regex3: ${simplify(regex3)}")
    println(s"Simplify regex4: ${simplify(regex4)}")
  }
}
