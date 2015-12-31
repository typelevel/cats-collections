package dogs

package object bedazzle {
  object option extends OptionSyntax
  object list extends ListBedazzle
  object birds extends BedazzleBirds
  object streaming extends StreamingSyntax
  object validated extends ValidatedSyntax
  object xor extends XorSyntax

  object all extends
      OptionSyntax with
      ListBedazzle with
      BedazzleBirds with
      StreamingSyntax with
      XorSyntax with
      ValidatedSyntax 
}
