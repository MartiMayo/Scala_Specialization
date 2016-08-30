package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    n <- arbitrary[A]
    h <- frequency((1, Gen.const(empty)), (9, genHeap))
  } yield insert(n, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min of two inserts") = forAll { (a: A, b: A) =>
    val h = insert(a, insert(b, empty))
    findMin(h) == Math.min(a, b)
  }

  property("Insert and delete into empty heap") = forAll { (a: A) =>
    isEmpty(deleteMin(insert(a, empty)))
  }

  property("Heap is sorted") = forAll{ (h: H) =>
    def sortedHeap(h: H): Boolean = {
      if (isEmpty(h)) true
      else {
        val min = findMin(h)
        val h2 = deleteMin(h)
        isEmpty(h2) || min <= findMin(h2) && sortedHeap(h2)
      }
    }
    sortedHeap(h)
  }

  property("Minimum of melding two heaps") = forAll {(h: H, g: H) =>
    findMin(meld(h,g)) == Math.min(findMin(h), findMin(g))
  }


  property("melding holding") = forAll{ (h: H, g: H) =>
    def isEqual(h2: H, g2: H): Boolean = {
      if (isEmpty(h2) && isEmpty(g2)) true
      else {
        val min_h = findMin(h2)
        val min_g = findMin(g2)
        min_h == min_g && isEqual(deleteMin(h2), deleteMin(g2))
      }
    }
    isEqual(meld(h, g), meld(deleteMin(h), insert(findMin(h), g) ))
  }

}
