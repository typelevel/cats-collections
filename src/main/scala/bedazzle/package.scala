package dogs

package object bedazzle {
  object option extends OptionBedazzle
  object list extends ListBedazzle
  object either extends EitherBedazzle
  object birds extends BedazzleBirds

  object all extends OptionBedazzle with ListBedazzle with BedazzleBirds
}
