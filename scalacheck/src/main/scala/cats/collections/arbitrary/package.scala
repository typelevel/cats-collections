package cats.collections

package object arbitrary {
  object all extends AllArbitrary

  object set extends ArbitrarySet
  object map extends ArbitraryMap
  object iset extends ArbitraryISet
  object cogen extends CogenInstances
}
