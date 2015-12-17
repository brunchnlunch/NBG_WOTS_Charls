package models

case class Product(ean: Long, name: String, volume: Double, quantity: Long) {
  def this(ean: Long, quantity: Long) = this(ean, "", 0, quantity)
}

object Product {
  var products = Set(Product(5010255079763L, "Christmas Gnome", 10.5, 20), 
	  Product(5018206244666L, "Fishing Gnome",  5.75, 30), 
	  Product(5018306332812L, "Big Gnome",  8.25, 50), 
	  Product(5018306312913L, "Swimming Gnome",  5, 35), 
	  Product(5018206244611L, "Purple Gnome", 4.5, 5), 
	  Product(5018306312914L, "Tiny Gnome",  7.75, 22),
	  Product(5018306318915L, "Robot Gnome", 8.85, 20),
	  Product(5018306333812L, "Disco Gnome", 5.5, 10),
	  Product(5018306319915L, "Happy Gnome", 6.67, 4))
/**
 * Return the products list ordered by ean number.
 */
	def findAll = products.toList.sortBy(_.ean)
	
	/*
	 * Given the ean of a specific product, returns the unique product with matching ean number
	 */
	def findByEan(ean: Long) = products.find(_.ean == ean)
	
	@deprecated("This function is not used", "14/12/15")
	def findItemByName (Name : String) : Product = {
    var a : Product = null
    for(item <- products) {
      if(item.name == Name){
        a = item
      }
    }
    a
  }
  
  /**
   * Given a ProductPart argument (an object which contains the ean, quantity and location of a product), decreases the quantity of the product with the corresponding ean number in the main set products.
   */
  def decreaseQuantity(product: ProductPart) {
    var newItems = Set.empty[Product]
    for(item <- products) {
      if(item.ean != product.ean) {
        newItems += item
      } else {
        newItems += new Product(item.ean, item.name, item.volume, item.quantity-product.quantity)
      }
    }
    products = newItems
  }
  
  /**
   * Given a ProductPart argument (an object which contains the ean, quantity and location of a product), increases the quantity of the product with the corresponding ean number in the main set products.
   */
  def increaseQuantity(product: ProductPart) {
    var newItems = Set.empty[Product]
    for(item <- products) {
      if(item.ean != product.ean) {
        newItems += item
      } else {
        newItems += new Product(item.ean, item.name, item.volume, item.quantity+product.quantity)
      }
    }
    products = newItems
  }
  
  /**
   * Returns the difference between the two given arguments
   */
  def calcQuant(a: Long, b: Long): Long = a - b
  
  
  /**
   * Given the ean number of a product and the quantity of stock required, this function checks to see if the current quantity of the product is above the required amount of quantity. Returns true if product is in stock and false otherwise.
   */
  def inStock(ean: Long, quantity: Long): Boolean ={
    def checkProduct(products: List[Product]): Boolean ={
      if(products.isEmpty){
        println("no more products")
        false
      } else if(products.head.ean.==(ean) && products.head.quantity.>=(quantity)) {
          println("product instock")
          true
        } else {
          checkProduct(products.tail)
        }
    }
    checkProduct(products.toList)
  }
}