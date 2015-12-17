import org.specs2.mutable._
import org.specs2.runner._
import org.junit.runner._

import play.api.test._
import play.api.test.Helpers._

/**
 * add your integration spec here.
 * An integration test will fire up a whole play application in a real (or headless) browser
 */
@RunWith(classOf[JUnitRunner])
class IntegrationSpec extends Specification {

  "Application" should {

    "work from within a browser" in new WithBrowser {

      browser.goTo("http://localhost:" + port)

      browser.pageSource must contain("Warehouse Order Tracking System")
    }
    
    "show products from within a browser" in new WithBrowser {

      browser.goTo("http://localhost:" + port + "/products")

      browser.pageSource must contain("Quantity")
    }
    
    "show damaged products from within a browser" in new WithBrowser {

      browser.goTo("http://localhost:" + port + "/products/damaged")

      browser.pageSource must contain("Product details")
    }
    
    "show individual products from within a browser" in new WithBrowser {

      browser.goTo("http://localhost:" + port + "/products/5010255079763")

      browser.pageSource must contain("Christmas Gnome")
    }
    
    "show orders from within a browser" in new WithBrowser {

      browser.goTo("http://localhost:" + port + "/orders")

      browser.pageSource must contain("Order Id")
    }
    
    "show assigned order from within a browser" in new WithBrowser {

      browser.goTo("http://localhost:" + port + "/orders/pickorder")

      browser.pageSource must contain("Address")
    }
    
    "show individual orders from within a browser" in new WithBrowser {

      browser.goTo("http://localhost:" + port + "/orders/1")

      browser.pageSource must contain("Address")
    }
    
    
  }
}
