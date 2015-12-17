import org.specs2.mutable._
import org.specs2.runner._
import org.junit.runner._
import play.api.test._
import play.api.test.Helpers._
import models.Order
import models.Product
import models.ProductPart
import util.OrderStatus
import models.PurchaseOrder
import models.Worker


@RunWith(classOf[JUnitRunner])
object MyTests extends Specification {
  
  "Application" should { 
    //Product functions--------------------------------------------------------
    "return product by ean" in new WithApplication{
      var product = Product.findAll
      assert(product.isInstanceOf[List[Product]])
    }
    
    "return a product given an ean number" in new WithApplication{
      var product = Product.findByEan(5010255079763L)
      var expectedProductName = "Christmas Gnome"
      var productName = product.get.name
      assert(expectedProductName == productName)
    }
    
    "increase quantity of a given product" in new WithApplication{
      var originalProductQuantity = Product.findByEan(5010255079763L).get.quantity
      var productPart = new ProductPart(5010255079763L, 1, "A2")
      Product.increaseQuantity(productPart)
      var ProductQuantity = Product.findByEan(5010255079763L).get.quantity
      assert(originalProductQuantity + 1 == ProductQuantity)
    }
    
    "check if we have enough stock given an order and an amount of stock" in new WithApplication{
      var inStock = Product.inStock(5010255079763L, 3)
      assert(inStock == true)
    }  
    //Order functions--------------------------------------------------------
    "return orders ordered by ID" in new WithApplication{
      var orders = Order.findAll
      assert(orders.isInstanceOf[List[Order]])
    }
    
    "return an order given an ID number" in new WithApplication{
      var order = Order.findById(1)
      var orderAddress = order.get.address
      var expectedOrderAddress = "Jade Pencil\nBrackley Road\nTiers Cross\nSA62 2AA"
      assert(orderAddress == expectedOrderAddress)
    }
    
    "print an order label" in new WithApplication{
      var orderLabel = Order.printLabel(1)
      assert(orderLabel.isInstanceOf[String])
    }
    
    "assign me an order" in new WithApplication{
      var order = Order.findNextOrder()
      assert(order.isInstanceOf[Order])
    }
    
    "update the status of an order" in new WithApplication{   //--------GOOD!!!!!!!
      Order.updateStatus(1, Order.premoteStatus)
      var order = Order.findById(1)
      assert(order.get.status.equals(OrderStatus.PROCESSING))
    }
    
    "check if an order is in stock" in new WithApplication{
      var inStock = Order.checkStock(List(new ProductPart(5010255079763L, 19)))
      assert(inStock == true)
    }
    
//    "dispatch an order" in new WithApplication{      -------------------------same as status order
//      
//    }
    
    "calculate volume of required box" in new WithApplication{
      var vol = Order.calcVolume(Order.orders.toList(0))
      assert(vol == 10.5 * 19)
    }
    //PurchaseOrder functions--------------------------------------------------------
    "return purchase orders ordered by date" in new WithApplication{
      var purchaseOrders = PurchaseOrder.findAll
      assert(purchaseOrders.isInstanceOf[List[PurchaseOrder]])
    }
    
    "return a purchase order given an id" in new WithApplication{
      var purchaseOrder = PurchaseOrder.findById(1).get
      assert(purchaseOrder.isInstanceOf[PurchaseOrder])
    }
    
//    "update a purchase order as received" in new WithApplication{   //same as status order
//      
//    }
    
//    "update a purchase order as processed" in new WithApplication{   //same as status order
//      
//    }
    //Worker functions----------------------------------------------------------------
    "return a worker given an ID number" in new WithApplication{
      var worker = Worker.findById(1).get
      assert(worker.isInstanceOf[Worker])
    }
    
    "allow me to clock in" in new WithApplication{
      var originalClockIn = Worker.findById(1).get.clockIn
      Thread.sleep(1000)
      Worker.clockIn(1)
      var clockIn = Worker.findById(1).get.clockIn
      assert(clockIn != originalClockIn)
    }
    
    "allow me to clock out" in new WithApplication{
      Worker.clockOut(1)
      var clockOut = Worker.findById(1).get.clockOut
      assert(clockOut != 0)
    }
    
    "check if a worker is at work" in new WithApplication{
      var isAtWork = Worker.isAtWork(2)
      assert(isAtWork == true)
    }
    
    
    
    
    
    
  }
}