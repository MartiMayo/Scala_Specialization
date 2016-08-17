/*
Prod function that takes a function f and returns
a function that given two parameters a and b
returns the production of f(i) for all i between a and b
 */
def prod(f: Int => Int)(a: Int, b: Int): Int = {
  if (a > b) 1
  else f(a) * prod(f)(a+1,b)
}

/* Factorial written in therms of prod is just
 * prod using the Identity as f(n) */
def fact(n: Int) = prod(x => x)(1, n)

fact(5)
fact(0)
fact(7)
fact(4)