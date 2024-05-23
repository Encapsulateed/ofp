class Multiset private (private val elements: Map[String, Int]) {

  def this() = this(Map.empty)

  // Добавить строку
  def +(str: String): Multiset = {
    new Multiset(elements + (str -> (elements.getOrElse(str, 0) + 1)))
  }

  // Объединить
  def +(other: Multiset): Multiset = {
    new Multiset(other.elements.foldLeft(elements) {
      case (acc, (str, count)) => acc + (str -> (acc.getOrElse(str, 0) + count))
    })
  }

  // Пересечь
  def *(other: Multiset): Multiset = {
    val newElements = elements.collect {
      case (str, count) if other.elements.contains(str) =>
        str -> math.min(count, other.elements(str))
    }
    new Multiset(newElements)
  }

  // Вычесть
  def -(other: Multiset): Multiset = {
    val newElements = elements.flatMap {
      case (str, count) if other.elements.contains(str) =>
        val newCount = count - other.elements(str)
        if (newCount > 0) Some(str -> newCount) else None
      case (str, count) => Some(str -> count)
    }
    new Multiset(newElements)
  }

  def contains(str: String): Boolean = elements.contains(str)

  def count(str: String): Int = elements.getOrElse(str, 0)

  def toList: List[String] = elements.toList.flatMap {
    case (str, count) => List.fill(count)(str)
  }

  // Переопределение метода toString для удобного вывода
  override def toString: String = elements.toString
}

object Multiset {
  def apply(strings: String*): Multiset = {
    val initialElements = strings.groupBy(identity).view.mapValues(_.size).toMap
    new Multiset(initialElements)
  }
}

object Main {
  def main(args: Array[String]): Unit = {

    val multiset1 = Multiset("a", "b", "a")
    val multiset2 = Multiset("a", "b", "c")

    println(multiset1 + multiset2) // Объединение
    println(multiset1 * multiset2) // Пересечение
    println(multiset1 - multiset2) // Вычитание
  }
}
