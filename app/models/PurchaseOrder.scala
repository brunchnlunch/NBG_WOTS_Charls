package models

import java.util.Date

case class PurchaseOrder(lines: Set[Product], date: Date, confirmed: Set[Product]) { }