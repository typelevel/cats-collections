package cats.collections

package object syntax {
  object all extends FoldableSyntax with RangeSyntax

  object foldable extends FoldableSyntax
  object range extends RangeSyntax
}
