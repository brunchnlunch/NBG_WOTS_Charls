package models

import java.util.Date
import util.OrderStatus

case class Order(id: Long, address: String, lines: Set[ProductPart], date: Int, status: OrderStatus) { }

object Order {
  val d = new Date
  var orders = Set(
    Order(1, "mansion house", Set(new ProductPart(5010255079763L, 19)), d.getDate, OrderStatus.PLACED),
    Order(2, "Aarons bedroom", Set(new ProductPart(5018306332812L, 10), new ProductPart(5018306312913L, 1)), d.getDate, OrderStatus.PLACED),
    Order(3, "Magic Party", Set(new ProductPart(5018206244666L, 3), new ProductPart(5018206244611L, 2), new ProductPart(5018306312914L, 1)), d.getDate, OrderStatus.PLACED))
  
  def findAll = orders.toList.sortBy(_.date).reverse
  
  def findById(id: Long) = orders.find(_.id == id)
  
  def findNextOrder(): Order ={
    def checkOrder(orders: List[Order]): Order ={
      if(orders.isEmpty) {
        null
      } else if(orders.head.status.==(OrderStatus.PLACED) && checkStock(orders.head.lines.toList)){
          orders.head
        } else {
          checkOrder(orders.tail)
        }
    }
    val tmp = orders.toList.sortBy(_.date).reverse
    checkOrder(tmp)
  }
  
  def demoteStatus(status: OrderStatus): OrderStatus = {
    status match {
      case OrderStatus.PICKED => OrderStatus.PROCESSING
      case OrderStatus.PACKED => OrderStatus.PICKED
      case OrderStatus.DISPATCHED => OrderStatus.PACKED
      case _ => OrderStatus.PLACED
    }
  }
  
  def premoteStatus(status: OrderStatus): OrderStatus ={
    status match {
      case OrderStatus.PLACED => OrderStatus.PROCESSING
      case OrderStatus.PROCESSING => OrderStatus.PICKED
      case OrderStatus.PICKED => OrderStatus.PACKED
      case OrderStatus.PACKED => OrderStatus.DISPATCHED
      case _ => OrderStatus.PLACED
    }
  }
  
  def updateStatus(order: Long, ss: (OrderStatus => OrderStatus)) {
    var tmp = Set.empty[Order]
    for(o <- orders) {
      if(o.id.==(order))
        tmp = tmp + new Order(order, o.address, o.lines, o.date, ss(o.status))
      else
        tmp = tmp + o
    }
    orders = tmp
  }
  
  def checkStock(lines: List[ProductPart]): Boolean ={
    def checkLine(lines: List[ProductPart]): Boolean ={
      if(lines.isEmpty) {
        true
      } else if(Product.inStock(lines.head.ean, lines.head.quantity)) {
          checkLine(lines.tail)
        } else {
          false
        }
    }
    checkLine(lines)
  }
  
  def dispatch {
    dispatcher(orders.toList)
    def dispatcher(orders: List[Order]) {
      if(orders.isEmpty)
        Unit
      else { 
        if(orders.head.status.equals(OrderStatus.PACKED))
          updateStatus(orders.head.id, premoteStatus(_))
        dispatcher(orders.tail)
      }
    }
  }
  
  def calcVolume(order: Order): Double ={
    var vol: Double = 0
    for(line <- order.lines) {
      vol = vol + (line.quantity * Product.findByEan(line.ean).get.volume)
    }
    vol
  }
}