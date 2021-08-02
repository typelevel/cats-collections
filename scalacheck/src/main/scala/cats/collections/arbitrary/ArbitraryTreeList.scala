package cats.collections
package arbitrary

import org.scalacheck.{Arbitrary, Cogen}

trait ArbitraryTreeList {
  implicit def arbitraryTreeList[A: Arbitrary]: Arbitrary[TreeList[A]] =
    Arbitrary(Arbitrary.arbitrary[List[A]].map(TreeList.fromListReverse(_)))

  implicit def cogenTreeList[A: Cogen]: Cogen[TreeList[A]] =
    Cogen[List[A]].contramap(_.toList)
}

object ArbitraryTreeList extends ArbitraryTreeList
