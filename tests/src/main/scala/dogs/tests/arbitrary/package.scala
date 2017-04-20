package dogs.tests

package object arbitrary {
  object all extends AllArbitrary

  object dlist extends ArbitraryDList
  object set extends ArbitrarySet
  object iset extends ArbitraryISet
  object streaming extends ArbitraryStreaming
  object vector extends ArbitraryVector
  object cogen extends CogenInstances
}
