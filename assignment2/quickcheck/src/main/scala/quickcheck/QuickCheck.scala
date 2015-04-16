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

  property("meld1") = forAll { (h: H, i: H) =>
    val hmin = findMin(h)
    val imin = findMin(i)

    if (hmin < imin) findMin(meld(h, i)) == hmin
    else findMin(meld(h, i)) == imin
  }

  property("val5") = forAll { (h: H) =>
    validate(findMin(h), deleteMin(h))
  }

  property("min2") = forAll { (a: Int, b: Int) =>
    val ah = insert(a, empty)
    val bh = insert(b, ah)

    if (a < b) findMin(bh) == a
    else findMin(bh) >= b
  }

  property("empty1") = forAll { a: Int =>
    val h = insert(a, empty)
    val dh = deleteMin(h)

    isEmpty(dh)
  }

  property("meld2") = forAll { (h: H, i: H) =>
    val hi = meld(h, i)
    val ih = meld(i, h)

    heapEquals(hi, ih)
  }

  property("meld3") = forAll { (h: H, i: H) =>
    val hi = meld(h, i)
    val ih = meld(i, h)

    (heapSize(h) + heapSize(i)) == heapSize(hi) && (heapSize(h) + heapSize(i)) == heapSize(ih)
  }

  property("meld4") = forAll { (h: H, i: H) =>
    val hi = meld(h, i)
    val ih = meld(i, h)

    val hiih = meld(hi, ih)
    val ihhi = meld(ih, hi)

    validate(findMin(hiih), deleteMin(hiih)) && validate(findMin(ihhi), deleteMin(ihhi))
  }

  property("meld5") = forAll { (h: H, i: H) =>
    val hi = meld(h, i)
    val ih = meld(i, h)

    val hiih = meld(hi, ih)
    val ihhi = meld(ih, hi)

    heapEquals(hiih, ihhi)
  }

  property("order1") = forAll { l: List[Int] =>
    val h = fillHeap(l)
    val ls = l.sortWith((a: Int, b: Int) => a < b)

    compareWithList(h, ls)
  }

  def validate(p: Int, heap: H): Boolean = {
    if (isEmpty(heap)) return true
    else findMin(heap) >= p && validate(findMin(heap), deleteMin(heap))
  }

  def heapEquals(h: H, i: H): Boolean = {
    if (isEmpty(h) && isEmpty(i)) return true
    else if (isEmpty(h) || isEmpty(i)) return false
    else findMin(h) == findMin(i) && heapEquals(deleteMin(h), deleteMin(i))
  }

  def heapSize(h: H): Int = {
    if (isEmpty(h)) 0 else 1 + heapSize(deleteMin(h))
  }

  def fillHeap(l: List[Int]): H = l match {
    case Nil => empty
    case x::xs => insert(x, fillHeap(xs))
  }

  def compareWithList(h: H, l: List[Int]): Boolean = {
    if (isEmpty(h) && l.isEmpty) true
    else if (isEmpty(h) || l.isEmpty) false
    else findMin(h) == l.head && compareWithList(deleteMin(h), l.tail)
  }

  lazy val genHeap: Gen[H] = for {
    a <- arbitrary[Int]
    h <- if (a % 10 == 0) const(empty) else genHeap
  } yield insert(a, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
