package models

case class Packaging(box: String, volume: Double) { }

object Packaging {
  var boxes = Set(Packaging("A1", 5), Packaging("A2", 10), Packaging("A3", 15), Packaging("A4", 20),
      Packaging("B1", 12), Packaging("B2", 24), Packaging("B3", 36), Packaging("B4", 48),
      Packaging("C1", 50), Packaging("C2", 100), Packaging("C3", 150), Packaging("C4", 200))
      
  def findAll = boxes.toList.sortBy(_.volume)
  
  def selectBox(volume: Double): List[Packaging] ={
    def selectEachBox(volume: Double, boxList: List[Packaging]): List[Packaging] ={
      if(volume.<=(0))
        boxList
      else if(addBox(volume).equals(null)) {
          var tmp = addBox(200) :: boxList
          selectEachBox(volume.-(200), tmp)
        } else {
          selectEachBox(volume-volume, addBox(200) :: boxList)
        }
    }
    selectEachBox(volume, List.empty[Packaging])  
  }
  
  def addBox(volume: Double): Packaging ={
    def checkboxes(boxes: List[Packaging]): Packaging ={
      if(boxes.isEmpty)
        null
      else {
         println(">>> Box too small?: " + boxes.head.volume.>=(volume))
        if(boxes.head.volume.>=(volume)) {
           println(">>> Check next box")
           checkboxes(boxes.tail)
        }
        else {
          println(">>> Box found: " + boxes.head.box)
          boxes.head
        }  
      }
    }
    checkboxes(boxes.toList.sortBy(_.volume))
  }
}