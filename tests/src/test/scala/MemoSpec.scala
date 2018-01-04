package dogs
package tests

import org.scalacheck._

class MemoSpec extends DogsSuite { msThis =>
  import Memo._

  /* Lists of memos that are thread safe */
  val safeMapMemos = List(immutableDefaultMapMemo, immutableListMapMemo, immutableHashMapMemo, concurrentTrieMapMemo)
  val safeOtherMemos = List(nilMemo, lastValueMemo, concurrentWeakHashMapMemo)
  val safeMemos = safeMapMemos ++ safeOtherMemos

  /* Lists of memos that are not thread safe, except arrayMemo which requires an array type. */
  val unsafeMapMemos = List(mutableDefaultMapMemo, mutableHashMapMemo)
  val unsafeOtherMemos = List(mutableLastValueMemo, mutableWeakHashMapMemo)
  val unsafeMemos = unsafeMapMemos ++ unsafeOtherMemos

  /* Validate that none of our memos change the values of functions */
  test("Memo doesn't change values") {
    for {
      (memos, sync) <- Seq((safeMemos, false), (unsafeMemos :+ arrayMemo[String](30), true))
      memo          <- memos
    } {
      forAll { (f: Int => String) =>
        val fm = memo(f)
        forAll { i: Int =>
          def test() = fm(i) shouldEqual f(i)
          if (sync) {
            msThis.synchronized { test() }
          } else {
            test()
          }
        }
      }
    }
  }
  test("Memo prevents repeated calls") {
    val permanentMemos = safeMapMemos ++ unsafeMapMemos :+ arrayMemo[java.lang.Integer](10)
    for (memo <- permanentMemos) {
      var calls = 0
      def fn(i: Int): java.lang.Integer = { calls += 1; i }
      val mfn = memo(fn)
      def call(n: Int): Unit = { mfn(n) shouldEqual n; () }
      calls shouldEqual 0
      call(0)
      calls shouldEqual 1
      call(0)
      calls shouldEqual 1
      call(1)
      calls shouldEqual 2
      call(0)
      calls shouldEqual 2
      call(5)
      calls shouldEqual 3
      call(1)
      calls shouldEqual 3
      call(5)
      calls shouldEqual 3
    }
  }
  test ("LastValue memo works") {
    for (memo <- Seq(lastValueMemo, mutableLastValueMemo)) {
      var calls = 0
      def fn(i: Int) = { calls += 1; i }
      val mfn = memo(fn)
      def call(n: Int): Unit = { mfn(n) shouldEqual n; () }
      calls shouldEqual 0
      call(0)
      calls shouldEqual 1
      call(0)
      calls shouldEqual 1
      call(0)
      calls shouldEqual 1
      call(0)
      calls shouldEqual 1
      call(1)
      calls shouldEqual 2
      call(0)
      calls shouldEqual 3
      call(1)
      calls shouldEqual 4
      call(1)
      calls shouldEqual 4
      call(1)
      calls shouldEqual 4
      call(1)
      calls shouldEqual 4
      call(2)
      calls shouldEqual 5
      call(0)
      calls shouldEqual 6
      call(0)
      calls shouldEqual 6
    }
  }
}
