package cats.collections

package object syntax {
  object all extends
      BedazzleBirds   with
      FoldableSyntax  with
      RangeSyntax     with
      StreamingSyntax

  object birds     extends BedazzleBirds
  object foldable  extends FoldableSyntax
  object range     extends RangeSyntax
  object streaming extends StreamingSyntax
}
