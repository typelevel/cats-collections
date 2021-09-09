package cats.collections

import cats.Eval
import munit.DisciplineSuite
import org.scalacheck._
import org.scalacheck.Prop._

class AnotherDietSuite extends DisciplineSuite {
  import DietSuite._

  override def scalaCheckTestParameters: Test.Parameters =
    DefaultScalaCheckPropertyCheckConfig.default
      .withMinSuccessfulTests(if (BuildInfo.isJvm) 300 else 30)

  property("foldLeft")(forAll { (rs: Ranges, start: Int, f: (Int, Int) => Int) =>
    assertEquals(rs.toDiet.foldLeft(start)(f), rs.toSet.toList.sorted.foldLeft(start)(f))
  })

  property("foldLeft/toList")(forAll { (rs: Ranges) =>
    assertEquals(rs.toDiet.foldLeft(List.empty[Int])(_ :+ _), rs.toDiet.toList)
  })

  property("foldRight")(forAll { (rs: Ranges, start: Int, f: (Int, Int) => Int) =>
    assertEquals(rs.toDiet.foldRight(Eval.now(start))((v, acc) => acc.map(f(v, _))).value,
                 rs.toSet.toList.sorted.foldRight(start)(f)
    )
  })

  property("foldRight/toList")(forAll { (rs: Ranges) =>
    assertEquals(
      rs.toDiet.foldRight(Eval.now(List.empty[Int]))((v, acc) => acc.map(v :: _)).value,
      rs.toDiet.toList
    )
  })

  property("not be modified when inserting existing item")(forAll { (d: Diet[Int]) =>
    d.toList.forall(elem =>
      // there may be structural changes, so fall back to list comparison
      d.add(elem).toList == d.toList
    )
  })

  property("--")(forAll { (d1: Diet[Int], d2: Diet[Int]) =>
    val d = d1 -- d2
    d2.toList.foreach(elem => assert(!d.contains(elem)))
    d1.toList.foreach(elem => assert(d2.contains(elem) || d.contains(elem)))
  })
}
