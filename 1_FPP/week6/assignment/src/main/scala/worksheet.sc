def isPrime(n: Int): Boolean =
Range(2,n-1).forall(x => n % x != 0)

isPrime(2)

isPrime(12)

isPrime(17)

isPrime(25)



def scalarProduct(xs: List[Int], ys: List[Int]): Int =
  (for (
    (x,y) <- xs zip ys
  ) yield (x*y)).sum

scalarProduct(List(1,2,3), List(2,4,5))



/***************************************************/

def queens(n: Int): Set[List[Int]] = {
  def placeQueens(k :Int): Set[List[Int]] =
    if (k == 0) Set(List())
    else
      for {
        queens <- placeQueens(k - 1)
        col <- 0 until n
        if isSafe(col, queens)
      } yield col :: queens
    placeQueens(n)
}

def isSafe(col: Int, queens: List[Int]): Boolean = {
  val row = queens.length
  val queensWithRow = (row - 1 to 0 by -1) zip queens
  queensWithRow forall {
    case (r, c) => col != c && math.abs(col - c) != row - r
  }
}

def show(queens: List[Int]) = {
  val lines =
    for (col <- queens.reverse)
      yield Vector.fill(queens.length)("* ").updated(col, "X ").mkString
  for (l <- lines) yield println(lines)
}

queens(4) map show

/******************************************************/

