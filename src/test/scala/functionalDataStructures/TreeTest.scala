package functionalDataStructures

import org.scalatest.{FlatSpec, Matchers}


class TreeTest extends FlatSpec with Matchers {

    "size(Leaf(42))" should "be 1" in {
        Tree.size(Leaf(42)) should be (1)
    }

    "size(Branch(Leaf(13), Leaf(11)))" should "be 3" in {
        Tree.size(Branch(Leaf(13), Leaf(11))) should be (3)
    }

    "size(Branch(Branch(Leaf(67), Leaf(35)), Leaf(11)))" should "be 5" in {
        Tree.size(Branch(Branch(Leaf(67), Leaf(35)), Leaf(11))) should be (5)
    }

    "maximum(Leaf(42))" should "be 42" in {
        Tree.maximum(Leaf(42)) should be (42)
    }

    "maximum(Branch(Leaf(13), Leaf(11)))" should "be 13" in {
        Tree.maximum(Branch(Leaf(13), Leaf(11))) should be (13)
    }

    "maximum(Branch(Branch(Leaf(67), Leaf(35)), Leaf(11)))" should "be 67" in {
        Tree.maximum(Branch(Branch(Leaf(67), Leaf(35)), Leaf(11))) should be (67)
    }

    "depth(Leaf(42))" should "be 1" in {
        Tree.depth(Leaf(42)) should be (1)
    }

    "depth(Branch(Leaf(13), Leaf(11)))" should "be 2" in {
        Tree.depth(Branch(Leaf(13), Leaf(11))) should be (2)
    }

    "depth(Branch(Branch(Leaf(67), Leaf(35)), Leaf(11)))" should "be 3" in {
        Tree.depth(Branch(Branch(Leaf(67), Leaf(35)), Leaf(11))) should be (3)
    }

}