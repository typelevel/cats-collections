package dogs.tests.arbitrary

trait AllArbitrary
    extends ArbitraryDList
    with ArbitraryList
    with ArbitrarySet
    with ArbitraryISet
    with ArbitraryOption
    with ArbitraryStreaming
    with ArbitraryVector
    with CogenInstances
