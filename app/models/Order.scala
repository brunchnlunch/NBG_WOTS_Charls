package models

import java.util.Date
import util.OrderStatus
import java.util.Calendar

case class Order(id: Long, address: String, lines: Set[ProductPart], date: Int, status: OrderStatus) { }

object Order {
  val d = new Date().getTime()/1000
  var orders = Set(
    Order(1, "Jade Pencil\nBrackley Road\nTiers Cross\nSA62 2AA", Set(new ProductPart(5010255079763L, 19)), d.toInt, OrderStatus.PLACED),
    Order(2, "Imogen Faulkner \n95 South Western Terrace \nMinsterley \nSY5 3QA", Set(new ProductPart(5018306332812L, 10), new ProductPart(5018306312913L, 1)), d.toInt, OrderStatus.PLACED),
    Order(3, "Oliver Fox \n18 Davids Lane \nStubbins \nBL0 7ZJ", Set(new ProductPart(5018206244666L, 3), new ProductPart(5018206244611L, 2), new ProductPart(5018306312914L, 1)), d.toInt, OrderStatus.PLACED))
  
    /**
     * Return the orders list ordered by least recent first.
     */
  def findAll = orders.toList.sortBy(_.date).reverse
  
  /**
   * Given the id of an order, returns the unique order with corresponding id.
   */
  def findById(id: Long) = orders.find(_.id == id)
  
  /**
   * Given the id of an order, returns a String which contains all variables an order object as well as the date delivered
   */
  def printLabel(id: Long): String ={
    val order = findById(id).get
    "Order no. " + id + "\n\n" + order.address + "\n\n" + "Date Placed: " + order.date + "\n Date delivered: " + new Date((new Date().getTime()/1000)*1000L)
  }
  
  /**
   * Returns an order which has not yet been assigned and has all its products in stock.
   */
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
  
  /**
   * Given an order status, returns the next status in the processing sequence
   */
  def demoteStatus(status: OrderStatus): OrderStatus = {
    status match {
      case OrderStatus.PICKED => OrderStatus.PROCESSING
      case OrderStatus.PACKED => OrderStatus.PICKED
      case OrderStatus.DISPATCHED => OrderStatus.PACKED
      case _ => OrderStatus.PLACED
    }
  }
  
  /**
   * Given an order status, returns the next status in the processing sequence
   */
  def premoteStatus(status: OrderStatus): OrderStatus ={
    status match {
      case OrderStatus.PLACED => OrderStatus.PROCESSING
      case OrderStatus.PROCESSING => OrderStatus.PICKED
      case OrderStatus.PICKED => OrderStatus.PACKED
      case OrderStatus.PACKED => OrderStatus.DISPATCHED
      case _ => OrderStatus.PLACED
    }
  }
  
  /**
   * Given an order status, returns the previous status in the processing sequence
   */
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
  
  /**
   * Given a list of ProductParts, i.e. products with ean numbers with required quantity, checks if each product is in stock and returns true if all products are in stock and flase otherwise.
   */
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
  
  /**
   * Checks each order in the orders list to see if an order is of status "PACKED" and premotes that order status to DISPATCHED
   */
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
  
  /**
   * Given an order, sums up the volume of each product in the order, multiplied by the quantity of each product giving total volume of an order.
   */
  def calcVolume(order: Order): Double ={
    def addVoll(lines: List[ProductPart], vol: Double) : Double ={
      if(lines.isEmpty)
        vol
      else {
        addVoll(lines.tail, vol+(Product.findByEan(lines.head.ean).get.volume*lines.head.quantity))
      }
    }
    addVoll(order.lines.toList, 0)
  }
}