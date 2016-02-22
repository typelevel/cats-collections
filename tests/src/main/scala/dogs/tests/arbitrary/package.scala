package dogs.tests

package object arbitrary {
  object all extends AllArbitrary

  object const extends ArbitraryConst
  object dlist extends ArbitraryDList
  object eval extends ArbitraryEval
  object ior extends ArbitraryIor
  object list extends ArbitraryList
  object option extends ArbitraryOption
  object streaming extends ArbitraryStreaming
  object validated extends ArbitraryValidated
  object xor extends ArbitraryXor
}
