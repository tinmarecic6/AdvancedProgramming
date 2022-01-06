// Andrzej Wąsowski, IT University of Copenhagen

object MyModule {

  def abs(n: Int): Int = if (n < 0) -n else n

  /* Exercise 1 */
  def square (n: Int): Int = n*n

  private def formatAbs(x: Int) =
    s"The absolute value of $x is ${abs (x)}"

  val magic :Int = 42
  var result :Option[Int] = None

  def main(args: Array[String]): Unit = {
    assert (magic - 84 == magic.-(84))
    println (formatAbs (magic-100))
    println(square(magic))
  }
  def fib (n: Int): Int = if (n==1) 0 else if(n==2) 1 else fib(n-1) + fib(n-2)
}
