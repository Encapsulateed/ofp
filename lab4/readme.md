% Лабораторная работа № 4 «Case-классы и сопоставление с образцом в Scala»
% 23 мая 2024 г.
% Алексей Митрошкин, ИУ9-61Б

# Цель работы
Целью данной работы является приобретение навыков разработки case-классов на языке Scala
 для представления абстрактных синтаксических деревьев.
# Индивидуальный вариант

Регулярное выражение описано следующим абстрактным синтаксисом:
```
RegEx → ∅ | ε | SYMBOL | RegEx ⋃ RegEx | RegEx · RegEx | RegEx* 
```
Здесь ∅ — пустое множество, ⋃ — объединение, · — конкатенация.

Требуется написать функцию simplify : RegEx[T] => RegEx[T], упрощающую регулярное выражение:
```
r∅ = ∅r = ∅
r ∪ ∅ = ∅ ∪ r = r
rε = εr = r
ε* = ∅* = ε
```
Параметр T — тип символов алфавита.

# Реализация

```scala
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

```

# Тестирование

Вывод на std_out
```
Simplify regex1: Symbol(a)
Simplify regex2: Symbol(a)
Simplify regex3: Epsilon
Simplify regex4: EmptySet

```

# Вывод
Я познакомился с case классами в scala.
Понял на чём нужно был разрабатывать лабораторную номер 2 по теории формальных языков @TonitaN.