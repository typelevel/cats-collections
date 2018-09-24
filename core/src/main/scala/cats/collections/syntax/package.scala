package cats.collections

package object syntax {
  object all extends
      BedazzleBirds   with
      FoldableSyntax  with
      RangeSyntax     with
      StreamingSyntax

  object foldable  extends FoldableSyntax
  object range     extends RangeSyntax

  @deprecated("Streaming is obsolete. Use either fs2, Monix, or iteratees.", "cats-collections 0.7.0")
  object streaming extends StreamingSyntax
  @deprecated("Bird operators are provided by mouse", "cats-collections 0.7.0")
  object birds     extends BedazzleBirds
}
