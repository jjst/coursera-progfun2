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

  def popAll(heap: H): List[A] = {
    if(isEmpty(heap)) {
      Nil
    } else {
      findMin(heap) :: popAll(deleteMin(heap))
    }
  }

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

  property("heap in which element is inserted should not be empty") = forAll { (heap: H, elem: Int) =>
    !isEmpty(insert(elem, heap))
  }

  property("constantly finding and deleting the minima should give sorted list") = forAll { heap: H =>
    val elems = popAll(heap)
    elems == elems.sorted
  }

  property("melded heaps should give back elements from 2 original lists") = forAll { (h1: H, h2: H) =>
    popAll(meld(h1, h2)) == (popAll(h1) ++ popAll(h2)).sorted
  }
}
