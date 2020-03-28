package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop.{BooleanOperators => _, _}

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = 
    for 
      i <- arbitrary[Int]
      h <- oneOf(const(empty), genHeap)
    yield insert(i, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen0") = forAll { (a: Int) =>
    val h = insert(a, empty)
    findMin(h) == a
  }  

  property("gen1") = forAll { (h: H) =>
    val m = if isEmpty(h) then 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("gen2") = forAll {
    (x: Int, y: Int) => 
      val heap = insert(x, insert(y, empty))
      findMin(heap) == Math.min(x, y)
  } 

  property("gen3") = forAll {
    (x: Int) => 
      val heap = insert(x, empty)
      deleteMin(heap) == empty
  }

  property("gen4") = forAll {
    (h: H) =>
      def sub(acc: List[Int], heap: H): List[Int] =
        if isEmpty(heap) then acc
        else sub(findMin(heap) :: acc, deleteMin(heap))
      sub(List(), h).reverse == sub(List(), h).sorted
  }

  property("gen5") = forAll {
    (h1: H, h2: H) =>
      findMin(meld(h1, h2)) == Math.min(findMin(h1), findMin(h2))
  }

  property("gen6") = forAll {
    (x: Int, y: Int, z: Int) =>
      val heap = meld(insert(x, empty), insert(y, insert(z, empty)))
      findMin(heap) == Math.min(Math.min(x, y), z)
  }

  property("gen7") = forAll {
    (x: Int) => 
      val h = insert(x, empty)
      isEmpty(deleteMin(meld(h, empty)))
  }

  property("gen8") = forAll {
    (h1: H, h2: H) =>
      def sub(acc: List[Int], heap: H): List[Int] =
        if isEmpty(heap) then acc
        else sub(findMin(heap) :: acc, deleteMin(heap))
      sub(List(), meld(h1, h2)) == sub(List(), meld(insert(findMin(h1), h2), deleteMin(h1)))
  }
}