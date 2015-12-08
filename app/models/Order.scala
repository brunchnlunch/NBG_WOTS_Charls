package models

import java.util.Date

case class Order(address: String, lines: Set[Product], date: Date) { }