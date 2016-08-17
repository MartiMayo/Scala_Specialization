package polymorphism

/**
  * Created by marti on 12/08/2016.
  */
trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
  def nth(n: Int): T
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false
  def nth(n: Int): T =
    if (n == 0) head
    else tail.nth(n-1)
}

class Nil[T] extends List[T] {
  def isEmpty = true
  def head = throw new NoSuchElementException("Nil.head")
  def tail = throw new NoSuchElementException("Nil.tail")
  def nth(n: Int) = throw new IndexOutOfBoundsException("The list does not have that many elements")
}


