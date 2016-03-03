package dogs

package object syntax {
  object all extends
      BedazzleBirds   with
      ListBedazzle    with
      OptionSyntax    with
      RangeSyntax     with
      StreamingSyntax

  object birds     extends BedazzleBirds
  object list      extends ListBedazzle
  object option    extends OptionSyntax
  object range     extends RangeSyntax
  object streaming extends StreamingSyntax
}
