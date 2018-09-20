package cats.collections.tests.arbitrary

trait AllArbitrary
    extends ArbitraryDList
    with ArbitrarySet
    with ArbitraryISet
    with ArbitraryStreaming
    with CogenInstances
