@main
def main(): Unit = {
  println(fib(100, x => x % 2 != 0))
}

def fib(limit: Int, predicate: Int => Boolean): List[Int] = {
  def generateFib(a: Int, b: Int, acc: List[Int]): List[Int] = {
    if (a > limit) acc
    else if (predicate(a)) generateFib(b, a + b, a :: acc)
    else generateFib(b, a + b, acc)
  }
  generateFib(0, 1, Nil).reverse
}

