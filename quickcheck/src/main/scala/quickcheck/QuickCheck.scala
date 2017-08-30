package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    a <- arbitrary[A]
    h <- oneOf(const(empty),genHeap)
  } yield insert(a,h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  def getAll(h: H,accu: List[A] = List()): List[A] = {
    if(isEmpty(h)) accu
    else getAll(deleteMin(h),findMin(h) :: accu)
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min") = forAll { (a1: A, a2: A) =>
    findMin(insert(a2,insert(a1,empty))) == (if(a1 < a2) a1 else a2)
  }

  property("delete 1 element heap") = forAll { (a: A) =>
    isEmpty(deleteMin(insert(a,empty)))
  }

  property("sorted when deleting") = forAll { (h: H) =>

    def loop(h: H,accu: List[A]): List[A] = {
      if(isEmpty(h)) accu
      else {
        val m = findMin(h)
        loop(deleteMin(h),m :: accu)
      }

    }

    val out = loop(h, Nil)
    out == out.sorted.reverse
  }

  property("insert is min at some point") = forAll { (h: H, a: A) =>
    val insertedH = insert(a, h)
    getAll(insertedH).toSet.contains(a)
  }

  property("melding") = forAll { (h1: H, h2: H) =>
    val m1 = findMin(h1)
    val m2 = findMin(h2)
    val fm = findMin(meld(h1,h2))
    fm == (if(m1 < m2) m1 else m2)
  }

  property("meld then delete") = forAll { (h1: H, h2: H) =>
    val m1 = findMin(h1)
    val m2 = findMin(h2)
    val m = meld(h1, h2)

    def loop(h: H,accu: List[A]): List[A] = {
      if(isEmpty(h)) accu
      else {
        val m = findMin(h)
        loop(deleteMin(h),m :: accu)
      }

    }

    val out = loop(m, Nil)
    out == out.sorted.reverse
  }
}
