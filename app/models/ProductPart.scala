package models

case class ProductPart(ean: Long, quantity: Long, location: String) {
  def this(ean: Long, quantity: Long) = this(ean, quantity, "")
}