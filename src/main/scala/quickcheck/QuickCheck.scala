package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Math._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("empty") = forAll { a: Int =>
    isEmpty(empty)
  }

  property("findMin1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("findMin2") = forAll { (a: Int, b: Int) =>
    val h1 = insert(a, empty)
    val h2 = insert(b, h1)

    if (a > b ){
      findMin(h2) == b
    } else {
      findMin(h2) == a
    }
  }


  property("deleteMin1") = forAll { (a: Int, b: Int)  =>
    val h1 = insert(a, empty)
    val h2 = insert(b, h1)
    val h3 = deleteMin(h2)
    if (a > b ){
      findMin(h3) == a
    } else {
      findMin(h3) == b
    }
  }

  property("deleteMin2") = forAll { a: Int =>
    val h1 = insert(a, empty)
    val h3 = deleteMin(h1)
    isEmpty(h3)
  }

  property("meld") = forAll { (a: Int, b: Int) =>
    val h1 = insert(a, empty)
    val h2 = insert(b, empty)
    val h3 = meld(h1, h2)
    if (a > b ){
      findMin(h3) == b
    } else {
      findMin(h3) == a
    }
  }

  property("hint1") = forAll { (n1: A, n2: A) =>
    val h = insert(n1, insert(n2, empty))
    findMin(h) == min(n1, n2)
  }

  property("hint2") = forAll { (n: A) =>
    isEmpty(deleteMin(insert(n, empty)))
  }

  property("hint3") = forAll { (h: H) =>
    def isSorted(h: H): Boolean =
      if (isEmpty(h)) true
      else {
        val m = findMin(h)
        val h2 = deleteMin(h)
        isEmpty(h2) || (m <= findMin(h2) && isSorted(h2))
      }
    isSorted(h)
  }

  property("hint4") = forAll { (h1: H, h2: H) =>
    findMin(meld(h1, h2)) == min(findMin(h1), findMin(h2))
  }

  /* https://class.coursera.org/reactive-001/forum/thread?thread_id=97#post-371 */
  property("meld") = forAll { (h1: H, h2: H) =>
    def heapEqual(h1: H, h2: H): Boolean =
      if (isEmpty(h1) && isEmpty(h2)) true
      else {
        val m1 = findMin(h1)
        val m2 = findMin(h2)
        m1 == m2 && heapEqual(deleteMin(h1), deleteMin(h2))
      }
    heapEqual(meld(h1, h2),
      meld(deleteMin(h1), insert(findMin(h1), h2)))
  }

  property("gen1") = forAll { h: H =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  lazy val genHeap: Gen[H] =  for {
    n <- arbitrary[A]
    h <- frequency((1, Gen.const(empty)), (9, genHeap))
  } yield insert(n, h)


  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
