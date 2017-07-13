
object Sorted {

    def sup(l: Int, r: Int): Boolean = l > r

    def inf(l: Char, r: Char): Boolean = l < r

    def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
        def go(i: Int, as: Array[A], ordered: (A, A) => Boolean): Boolean = {
            if (i >= as.length - 1) {
                true
            } else if (ordered(as(i), as(i + 1))) {
                go(i + 1, as, ordered)
            }
            else {
                false
            }
        }

        go(0, as, ordered)
    }

    def main(args: Array[String]): Unit = {
        println {
            if (isSorted(Array(3, 2, 1), sup)) true
            else false
        }
        println {
            if (isSorted(Array(2, 3, 1), sup)) true
            else false
        }
        println {
            if (isSorted(Array('2', '3', '1'), inf)) true
            else false
        }
        println {
            if (isSorted(Array('1', '2', '3'), inf)) true
            else false
        }
    }
}
