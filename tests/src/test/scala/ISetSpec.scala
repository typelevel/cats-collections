package dogs

import dogs.Predef._
import dogs.std.{intOrder,intEq}
import dogs.tests.arbitrary._
import org.scalacheck._
import org.scalacheck.Arbitrary.{arbitrary=>getArbitrary,_}
import org.scalacheck.Prop._

object ISetSpec extends Properties("ISet") with ArbitraryList {
  import scala.collection.immutable.{Set => SSet, Map => MMap}

  property("intersection works") = forAll { (as: List[Int], bs: List[Int]) =>
    val setA: ISet[Int] = Set.fromList(as).iset
    val setEven: ISet[Int] = ISet(_ % 2 == 0)

    val s1 = setA & setEven
    val s2 = setA intersection setEven

    bs.forall(b => (s1(b) == (as.contains(b) && (b % 2 == 0))) &&
                   (s2(b) == (as.contains(b) && (b % 2 == 0))))

  }

  property("union works") = forAll { (as: List[Int], bs: List[Int]) =>
    val setA: ISet[Int] = Set.fromList(as).iset
    val setEven: ISet[Int] = ISet(_ % 2 == 0)

    val s1 = setA | setEven
    val s2 = setA union setEven

    bs.forall(b => (s1(b) == (as.contains(b) || (b % 2 == 0))) &&
                   (s2(b) == (as.contains(b) || (b % 2 == 0))))

  }

  property("difference works") = forAll { (as: List[Int], bs: List[Int]) =>
    val setA: ISet[Int] = Set.fromList(as).iset
    val setEven: ISet[Int] = ISet(_ % 2 == 0)

    val s1 = setA - setEven
    val s2 = setA diff setEven

    bs.forall(b => (s1(b) == (as.contains(b) && (b % 2 != 0))) &&
                   (s2(b) == (as.contains(b) && (b % 2 != 0))))

  }

  property("negation works") = forAll { (as: List[Int], bs: List[Int]) =>
    val setA: ISet[Int] = Set.fromList(as).iset
    val setEven: ISet[Int] = ISet(_ % 2 == 0)

    val s1 = !(setA - setEven)

    bs.forall(b => (s1(b) != (as.contains(b) && (b % 2 != 0))))

  }
}
