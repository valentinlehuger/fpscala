package functionalDataStructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
    // ex 3.25
    def size[A](root: Tree[A]): Int = root match {
        case Leaf(_) => 1
        case Branch(l, r) => 1 + size(l) + size(r)
    }

    // ex 3.26
    def maximum(root: Tree[Int]): Int = root match {
        case Leaf(v) => v
        case Branch(l, r) => maximum(l) max maximum(r)
    }

    // ex 3.27
    def depth[A](root: Tree[A]): Int = root match {
        case Leaf(v) => 1
        case Branch(l, r) => 1 + (depth(l) max depth(r))
    }

    // ex 3.28
    def map[A,B](root: Tree[A])(f: A => B): Tree[B] = root match {
        case Leaf(v) => Leaf(f(v))
        case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }

    def fold[A,B](root: Tree[A])(f: A => B)(g: (B, B) => B): B = root match {
        case Leaf(n) => f(n)
        case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }

    def size2[A](root: Tree[A]): Int = fold(root)(_ => 1)( (l, r) => l + r + 1)

    def maximum2(root: Tree[Int]): Int = fold(root)(a => a)(_ max _)

    def depth2[A](root: Tree[A]): Int =  fold(root)(_ => 1)( (l, r) => 1 + (l max r))

    def map2[A,B](root: Tree[A])(f: A => B): Tree[B] =
        fold(root)(a => Leaf(f(a)): Tree[B])(Branch(_, _))

}
