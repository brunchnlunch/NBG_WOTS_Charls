package models

import java.util.Date
import util.OrderStatus

case class PurchaseOrder(id: Long, lines: Set[ProductPart], date: Int, status: OrderStatus, confirmed: Set[ProductPart]) { 
  def this(id: Long, lines: Set[ProductPart], date: Int, status: OrderStatus) = this(id, lines, date, status, Set.empty[ProductPart])
}

object PurchaseOrder {
  val d = new Date().getTime()/1000
  var purchaseOrders = Set(
      new PurchaseOrder(1, Set(new ProductPart(5010255079763L, 20)), d.toInt, OrderStatus.PLACED), 
      new PurchaseOrder(2, Set(new ProductPart(5018206244611L, 35), new ProductPart(5018306312914L, 18)), d.toInt, OrderStatus.PLACED))
  
  /**
   * Returns the list of purchase orders ordered by most recent first.
   */
  def findAll = purchaseOrders.toList.sortBy(_.date)
  
  /**
   * Given the id of a purchase order returns the unique purchase order with has the corresponding id.
   */
  def findById(id: Long) = purchaseOrders.find(_.id == id)
  
  /**
   * Given a purchase order id, changes the purchase order status from placed to received.
   */
  def receivedOrder (purchaseOrder : Int) {
    def assignOrder(purchaseOrders: Set[PurchaseOrder], checkedPurchaseOrders: Set[PurchaseOrder]): Set[PurchaseOrder] ={
      if (purchaseOrders.isEmpty) {
        checkedPurchaseOrders
      } else {
        if (purchaseOrders.head.id.==(purchaseOrder)) {
          var cPO = checkedPurchaseOrders + new PurchaseOrder(purchaseOrders.head.id, purchaseOrders.head.lines, purchaseOrders.head.date, OrderStatus.RECEIVED)
          assignOrder(purchaseOrders.tail, cPO)
        } else {
          assignOrder(purchaseOrders.tail, checkedPurchaseOrders)
        }
      }
    }
    purchaseOrders = assignOrder(purchaseOrders, Set.empty[PurchaseOrder])
  }
  
  /**
   * Given a purchase order id and a set of product parts (ean, quantity and location) that you want to keep, adds products to the given purchase order and updates the purchase order status to processed.
   */
  def confirmOrder(purchaseOrder: Long, lines: Set[ProductPart]) {
    def assignOrder(purchaseOrders: Set[PurchaseOrder], checkedPurchaseOrders: Set[PurchaseOrder]): Set[PurchaseOrder] ={
      if (purchaseOrders.isEmpty) {
        checkedPurchaseOrders //products to keep
      } else {
        if (purchaseOrders.head.id.==(purchaseOrder)) {
          var cPO = checkedPurchaseOrders + PurchaseOrder(purchaseOrders.head.id, purchaseOrders.head.lines, purchaseOrders.head.date, OrderStatus.PROCESSED, lines)
          assignOrder(purchaseOrders.tail, cPO)
        } else {
          assignOrder(purchaseOrders.tail, checkedPurchaseOrders)
        }
      }
    }
    purchaseOrders = assignOrder(purchaseOrders, Set.empty[PurchaseOrder])
  }
}