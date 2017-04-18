package dogs

package object syntax {
  object all extends
      FoldableSyntax  with
      ListBedazzle    with
      OptionSyntax    with
      RangeSyntax     with
      StreamingSyntax

  object foldable  extends FoldableSyntax
  object list      extends ListBedazzle
  object option    extends OptionSyntax
  object range     extends RangeSyntax
  object streaming extends StreamingSyntax
}
