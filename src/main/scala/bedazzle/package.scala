package dogs

package object bedazzle {
  object option extends OptionBedazzle
  object list extends ListBedazzle

  object all extends OptionBedazzle with ListBedazzle
}
