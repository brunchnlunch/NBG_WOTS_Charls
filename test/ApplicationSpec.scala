import org.specs2.mutable._
import org.specs2.runner._
import org.junit.runner._

import play.api.test._
import play.api.test.Helpers._

/**
 * Add your spec here.
 * You can mock out a whole application including requests, plugins etc.
 * For more information, consult the wiki.
 */
@RunWith(classOf[JUnitRunner])
class ApplicationSpec extends Specification {

  "Application" should {

    "send 404 on a bad request" in new WithApplication{
      route(FakeRequest(GET, "/boum")) must beSome.which (status(_) == NOT_FOUND)
    }

    "render the index page" in new WithApplication{
      val home = route(FakeRequest(GET, "/")).get

      status(home) must equalTo(SEE_OTHER)
      //contentType(home) must beSome.which(_ == "text/html")
      //contentAsString(home) must contain ("Warehouse Order Tracking System")
    }
    
    "render the products list page" in new WithApplication{
      val list = route(FakeRequest(GET, "/products")).get

      status(list) must equalTo(OK)
      contentType(list) must beSome.which(_ == "text/html")
      contentAsString(list) must contain ("Quantity")
    }
    
    "render the damaged products page" in new WithApplication{
      val damages = route(FakeRequest(GET, "/products/damaged")).get

      status(damages) must equalTo(OK)
      contentType(damages) must beSome.which(_ == "text/html")
      contentAsString(damages) must contain ("Product details")
    }
    
    "render the individual products page" in new WithApplication{
      val prodPage = route(FakeRequest(GET, "/products/5010255079763")).get

      status(prodPage) must equalTo(OK)
      contentType(prodPage) must beSome.which(_ == "text/html")
      contentAsString(prodPage) must contain ("Christmas Gnome")
    }
    
    "render the orders list page" in new WithApplication{
      val orders = route(FakeRequest(GET, "/orders")).get

      status(orders) must equalTo(OK)
      contentType(orders) must beSome.which(_ == "text/html")
      contentAsString(orders) must contain ("Order Id")
    }
    
    "render the assigned order page" in new WithApplication{
      val assignedOrder = route(FakeRequest(GET, "/orders/pickorder")).get

      status(assignedOrder) must equalTo(OK)
      contentType(assignedOrder) must beSome.which(_ == "text/html")
      contentAsString(assignedOrder) must contain ("Address")
    }
    
    "render the individual orders page" in new WithApplication{
      val order = route(FakeRequest(GET, "/orders/1")).get

      status(order) must equalTo(OK)
      contentType(order) must beSome.which(_ == "text/html")
      contentAsString(order) must contain ("Address")
    }
    
//    no:
//    /barcode
//    /orders/:id/update 
//    post /products -----
//    /dispatching
    
  }
}
