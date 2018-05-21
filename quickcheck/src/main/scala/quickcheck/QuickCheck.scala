package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = {
    for {
      v <- arbitrary[A]
      m <- oneOf(const(empty), genHeap)
    } yield insert(v, m)
  }

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("two elements into an empty heap, findMin gives smaller elem") = forAll {
    (x: Int, y: Int) =>
      val heap = insert(x, insert(y, empty))
      findMin(heap) == math.min(x, y)
  }

  property("delete min of heap with one element heap is empty") = forAll {
    (a: Int) =>
      val heap = insert(a, empty)
      val del = deleteMin(heap)
      isEmpty(del)
  }

  property("using heaps for sorting") = forAll {
    heap: H =>
      def sortFromHeap(sorted: List[A], heap: H): List[A] = {
        val currentMin = findMin(heap)
        val deletedHeap = deleteMin(heap)
        if (!isEmpty(deletedHeap))
          sortFromHeap(currentMin :: sorted, deletedHeap)
        else currentMin :: sorted
      }

      val xs = sortFromHeap(Nil, heap)
      (xs, xs.tail).zipped.forall(_ >= _)
  }

  property("finding minimum on the melding heap") = forAll {
    (heap1: H, heap2: H) =>
      val min = math.min(findMin(heap1), findMin(heap2))
      findMin(meld(heap1, heap2)) == min
  }

  property("oddly melded lists are equal") = forAll { (h1: H, h2: H) =>
    def isHeapEqual(h1: H, h2: H): Boolean = {
      def isEqualIter(h1: H, h2: H, status: Boolean = true): Boolean = {
        if (isEmpty(h1))
          isEmpty(h2)
        else
          status && isEqualIter(deleteMin(h1), deleteMin(h2), findMin(h1) == findMin(h2))
      }
      isEqualIter(h1, h2)
    }

    isHeapEqual(meld(deleteMin(h1), insert(findMin(h1), h2)), meld(h1, h2))
  }
}

