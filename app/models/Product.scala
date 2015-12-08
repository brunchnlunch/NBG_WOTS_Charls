package models

case class Product(ean: Long, name: String, volume: Double, quantity: Long) {
  def this(ean: Long, quantity: Long) = this(ean, "", 0, quantity)
}

object Product {
  var products = Set(Product(5010255079763L, "Gareth Gnome", 10.5, 20), 
	  Product(5018206244666L, "James Gnompson",  5.75, 30), 
	  Product(5018306332812L, "Aaron Gnomeholland",  8.25, 50), 
	  Product(5018306312913L, "Gnomeharad pitty",  5, 35), 
	  Product(5018206244611L, "Mat Gnomeyard", 4.5, 5), 
	  Product(5018306312914L, "Devdatta Gnome",  7.75, 22))

	def findAll = products.toList.sortBy(_.ean)
	
	def findByEan(ean: Long) = products.find(_.ean == ean)
	
	def damaged(product: ProductPart) {
    var tmp = Set.empty[Product]
    for(p <- products) {
      if(p.ean.==(product.ean))
        tmp = tmp + new Product(product.ean, p.name ,p.volume, calcQuant(p.quantity, product.quantity))
      else
        tmp = tmp + p   
    }
    products = tmp
  }
  
  def calcQuant(a: Long, b: Long): Long = a - b
}