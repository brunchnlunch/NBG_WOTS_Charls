package models

import java.util.Date

case class PurchaseOrder(id: Long, lines: Set[Product], date: Date, confirmed: Set[Product]) { }