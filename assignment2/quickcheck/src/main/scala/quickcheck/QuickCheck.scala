package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import org.scalacheck.Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("meld3") = forAll { (h: H, i: H) =>
    val hmin = findMin(h)
    val imin = findMin(i)

    if (hmin < imin) findMin(meld(h, i)) == hmin
    else findMin(meld(h, i)) == imin
  }

  property("del2") = forAll { (h: H) =>
    validate(Int.MinValue, h)
  }

  def validate(p: Int, heap: H): Boolean = {
    if (!isEmpty(heap)) findMin(heap) >= p && validate(findMin(heap), deleteMin(heap))
    else true
  }

  lazy val genHeap: Gen[H] = for {
    a <- arbitrary[Int]
    h <- if (a % 10 == 0) const(empty) else genHeap
  } yield insert(a, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
