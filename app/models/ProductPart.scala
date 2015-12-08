package models

case class ProductPart(ean: Long, quantity: Long) { }

object ProductPart {
  var products = Set.empty[Product]
}