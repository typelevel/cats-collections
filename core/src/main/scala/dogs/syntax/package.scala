package dogs

package object syntax {
  object all extends
      FoldableSyntax  with
      RangeSyntax     with
      StreamingSyntax

  object foldable  extends FoldableSyntax
  object range     extends RangeSyntax
  object streaming extends StreamingSyntax
}
