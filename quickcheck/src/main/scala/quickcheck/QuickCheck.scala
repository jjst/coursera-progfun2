package quickcheck

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._

import scala.util.Try

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genEmptyHeap: Gen[H] = const(empty)

  lazy val genNonEmptyHeap: Gen[H] = for {
    v <- arbitrary[A]
    heap <- oneOf(genEmptyHeap, genEmptyHeap)
  } yield insert(v, heap)

  lazy val genHeap: Gen[H] = oneOf(genEmptyHeap, genNonEmptyHeap)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("inserting any two elements into empty heap should return minimum") = forAll { (elem1: Int, elem2: Int) =>
    val min = Math.min(elem1, elem2)
    val heap = insert(elem2, insert(elem1, empty))
    findMin(heap) == min
  }

  property("deleting minimum from one-element heap should result in empty heap") = forAll { (elem: Int) =>
    isEmpty(deleteMin(insert(elem, empty)))
  }

  property("finding the minimum of 2 merged heaps should return the minimum of one of them") = forAll { (heap1: H, heap2: H) =>
      val min = List(heap1, heap2).flatMap { h => Try(findMin(h)).toOption.toList }.sorted.headOption
      val meldedMin = Try(findMin(meld(heap1, heap2))).toOption
      meldedMin == min
  }
}
