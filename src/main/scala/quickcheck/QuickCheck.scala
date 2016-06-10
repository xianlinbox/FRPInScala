package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("empty") = forAll { a:Int =>
    isEmpty(empty)
  }

  property("findMin1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("findMin2") = forAll { a: Int =>
    val h1 = insert(a, empty)
    val h2 = insert(a + 1, h1)
    findMin(h2) == a
  }


  property("deleteMin1") = forAll { a: Int =>
    val h1 = insert(a, empty)
    val h2 = insert(a + 1, h1)
    val h3 = deleteMin(h2)
    findMin(h3) == a + 1
  }

  property("deleteMin2") = forAll { a: Int =>
    val h1 = insert(a, empty)
    val h3 = deleteMin(h1)
    isEmpty(h3)
  }

  property("meld") = forAll { a: Int =>
    val h1 = insert(a, empty)
    val h2 = insert(a + 1, empty)
    val h3 = meld(h1, h2)
    findMin(h3) == a
  }


  lazy val genHeap: Gen[H] = ???

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
