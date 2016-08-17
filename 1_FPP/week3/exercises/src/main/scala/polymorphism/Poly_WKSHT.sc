/************************************/
import polymorphism._

val list = new Cons(1, new Cons(2, new Cons(3, new Cons(4, new Nil))))
val elem = list.nth(2)
