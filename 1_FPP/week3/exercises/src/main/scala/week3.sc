class Rational(x: Int, y: Int) {
  require(y != 0, "denom must be nonzero")

  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  private val g = gcd(x,y)
  def numer = x / g
  def denom = y / g

  def less(that: Rational) = numer * that.denom < that.numer * denom

  def max(that: Rational) = if (this.less(that)) that else this

  def add(that: Rational) =
    new Rational(numer * that.denom + that.numer * denom,
      denom * that.denom)

  def sub(that: Rational) = add(that.neg)

  def neg = new Rational(-numer, denom)

  override def toString = numer + "/" + denom

}

val x = new Rational(1,2)
val y = new Rational(2,3)
val z = x.add(y)

x.sub(y).sub(z)


