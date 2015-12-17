package models

case class Shelf(product: ProductPart, shelfNo: String) { }

object Shelf {
  var shelves = Set(Shelf(new ProductPart(5010255079763L, 10), "A1"),
      Shelf(new ProductPart(5010255079763L, 10), "A2"),
      Shelf(new ProductPart(5018206244666L, 10), "A3"),
      Shelf(new ProductPart(5018206244666L, 10), "A4"),
      Shelf(new ProductPart(5018206244666L, 10), "A5"),
      Shelf(new ProductPart(5018306332812L, 10), "B1"),
      Shelf(new ProductPart(5018306332812L, 10), "B2"),
      Shelf(new ProductPart(5018306332812L, 10), "B3"),
      Shelf(new ProductPart(5018306332812L, 10), "B4"),
      Shelf(new ProductPart(5018306332812L, 10), "B5"),
      Shelf(new ProductPart(5018306312913L, 10), "C1"),
      Shelf(new ProductPart(5018306312913L, 10), "C2"),
      Shelf(new ProductPart(5018306312913L, 10), "C3"),
      Shelf(new ProductPart(5018306312913L, 5), "C4"),
      Shelf(new ProductPart(5018206244611L, 5), "C5"),
      Shelf(new ProductPart(5018306312914L, 10), "D1"),
      Shelf(new ProductPart(5018306312914L, 10), "D2"),
      Shelf(new ProductPart(5018306312914L, 2), "D3"),
      Shelf(new ProductPart(5018306318915L, 10), "D4"),
      Shelf(new ProductPart(5018306318915L, 10), "D5"),
      Shelf(new ProductPart(5018306333812L, 10), "E1"),
      Shelf(new ProductPart(5018306319915L, 4), "E2"))
      
  /**
   * Given the ean number of a product returns a list of Product Parts(ean, quantity) which have the same ean number.
   */
  def findByEan(ean: Long) ={
    def scanShelves(shelves: List[Shelf]): List[Shelf] ={
      if(shelves.isEmpty)
        shelves
      else {
        if(shelves.head.product.ean.==(ean))
          shelves.head :: scanShelves(shelves.tail)
        else
          scanShelves(shelves.tail)
      }
    }
    scanShelves(shelves.toList)
  }
  
  /**
   * Given an order returns a list of shelves of which the products of the order are and also decreased the values of the quantity of said products on each shelf.
   */
  def findForOrder(order: Order): List[List[Shelf]] ={
    def findForLines(lines: List[ProductPart], shelvesFound: List[List[Shelf]], fFL: ((ProductPart, List[Shelf], List[Shelf]) => List[Shelf])): List[List[Shelf]] ={
      if(lines.isEmpty) {
        println(">> Shelves Found")
        shelvesFound
      } else {
        println(">> Finding shelves for product " + lines.head.ean)
        printList(fFL(lines.head, shelves.toList, List.empty[Shelf]))
        val tmp = fFL(lines.head, shelves.toList, List.empty[Shelf]) :: shelvesFound
        printListMult(tmp)
        findForLines(lines.tail, tmp, fFL)
      }
    }
    findForLines(order.lines.toList, List.empty[List[Shelf]], findForLine(_, _, _))
  }
  
  /**
   * Given a product part (ean, quantity), the list of shelves and a found list (which should contain shelves where products are), returns a found list of shelves which contain the required product and reduces the quantity of product on that shelf.
   */
  def findForLine(line: ProductPart, shelf: List[Shelf], found: List[Shelf]): List[Shelf] ={
    if(shelf.isEmpty || line.quantity.<=(0)) {
      println(">> Shelves for product " + line.ean + " Found")
      found
    } else {
      if(shelf.head.product.ean.equals(line.ean) ) {
        println(">> Shelf found for product "  + line.ean + " at location " + shelf.head.shelfNo)
        removeFromShelf(shelf.head.shelfNo, calc(line.quantity, shelf.head.product.quantity))
        var pt = shelf.head :: shelf.tail
        println(">> current length: " + pt.length)
        findForLine(new ProductPart(line.ean, line.quantity-shelf.head.product.quantity), shelf.tail, shelf.head :: found)
      } else {
        println(">> Finishing Recursion")
        findForLine(line, shelf.tail, found)
      }
    }
  } //if you need to visit more than one shelf, doesn't tell you how many products to take off a given shelf?
  //sol: take all possible items off the first occuring shelf in the list.
  
  /**
   * Prints out a given list of a lists
   */
  def printListMult(locations: List[List[Shelf]]){
    println(">> Listing sheves found: ")
      for(location <- locations) {
        for(shelf <- location) {
          println(shelf.shelfNo + ", ")
        }
      }
  }
  
  /**
   * Prints out a given list
   */
  def printList(location: List[Shelf]){
    println(">> Listing shelvs " + location.length + " found : ")
    for(shelf <- location) {
      println(shelf.shelfNo + ", ")
    }
  }
  
  /**
   * Given a required quantity and a quantity on a shelf, return the quantity of required products if there are enough products on the shelf, otherwise return the maximum number of products which can be removed from the shelf. 
   */
  def calc(quant: Long, shelfquant: Long): Long ={
    if(shelfquant > quant)
      quant
    else
      shelfquant
  }
  
  /**
   * Given a shelf number and a quantity, reduces the number of a product on the given shelf by the given quantity
   */
  def removeFromShelf(shelf: String, quantity: Long) {
    var tmp = Set.empty[Shelf]
    for(s <- shelves) {
      if(s.shelfNo.equalsIgnoreCase(shelf))
        tmp = tmp + new Shelf(new ProductPart(s.product.ean, s.product.quantity-quantity), s.shelfNo)
      else
        tmp = tmp + s
    }
    shelves = tmp
  }
}