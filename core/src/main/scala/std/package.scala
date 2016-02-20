package dogs

import dogs.Predef._

package object std {
  import Order._

  implicit val bigIntEq: Eq[BigInt] = Eq.fromEquals
  implicit val bigDecimalEq: Eq[BigDecimal] = Eq.fromEquals
  implicit val booleanEq: Eq[Boolean] = Eq.fromEquals
  implicit val byteEq: Eq[Byte] = Eq.fromEquals
  implicit val charEq: Eq[Char] = Eq.fromEquals
  implicit val intEq: Eq[Int] = Eq.fromEquals
  implicit val longEq: Eq[Long] = Eq.fromEquals
  implicit val shortEq: Eq[Short] = Eq.fromEquals
  implicit val unitEq: Eq[Unit] = Eq.always

  implicit val bigIntOrder: Order[BigInt] = new Order[BigInt] {
    def apply(l: BigInt, r: BigInt) =
      if(l == r) EQ
      else if(l < r) LT
      else GT
  }

  implicit val bigDecimalOrder: Order[BigDecimal] = new Order[BigDecimal] {
    def apply(l: BigDecimal, r: BigDecimal) =
      if(l == r) EQ
      else if(l < r) LT
      else GT
  }

  implicit val byteOrder: Order[Byte] = new Order[Byte] {
    def apply(l: Byte, r: Byte) =
      if(l == r) EQ
      else if(l < r) LT
      else GT
  }

  implicit val charOrder: Order[Char] = new Order[Char] {
    def apply(l: Char, r: Char) =
      if(l == r) EQ
      else if(l < r) LT
      else GT
  }

  implicit val intOrder: Order[Int] = new Order[Int] {
    def apply(l: Int, r: Int) =
      if(l == r) EQ
      else if(l < r) LT
      else GT
  }

  implicit val longOrder: Order[Long] = new Order[Long] {
    def apply(l: Long, r: Long) =
      if(l == r) EQ
      else if(l < r) LT
      else GT
  }

  implicit val shortOrder: Order[Short] = new Order[Short] {
    def apply(l: Short, r: Short) =
      if(l == r) EQ
      else if(l < r) LT
      else GT
  }

  implicit val doubleOrder: Order[Double] = new Order[Double] {
    def apply(l: Double, r: Double) =
      if(l == r) EQ
      else if(l < r) LT
      else GT
  }

  implicit val floatOrder: Order[Float] = new Order[Float] {
    def apply(l: Float, r: Float) =
      if(l == r) EQ
      else if(l < r) LT
      else GT
  }

  implicit val stringOrder: Order[String] = new Order[String] {
    def apply(l: String, r: String) = l.compareTo(r) match {
      case 0 => EQ
      case x if x < 0 => LT
      case _ => GT
    }
  }
}

