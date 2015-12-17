package models

import java.util.Date

case class Worker (id: Long, name: String, clockIn: Int, clockOut: Int){
  def this(id: Long, name: String, clockIn: Int) = this(id, name, clockIn, 0)
}

object Worker {
  val d = new Date().getTime()/1000
  var workers = Set(new Worker(1, "Terry Bobbers", d.toInt),
      new Worker(2, "Joe Foot", d.toInt),
      new Worker(3, "Ted Desk", d.toInt))
    
  /**
   * Given a workers id, returns the unique worker with the corresponding id
   */
  def findById(id: Long) = workers.find(_.id == id)
  
  /**
   * Given a workers id, updates the clockIn attribute with the current time.
   */
  def clockIn(id: Long) {
    var newWorkers = Set.empty[Worker]
    def clockEmployee(workers: Set[Worker], newWorkers: Set[Worker]): Set[Worker] = {
      if (workers.isEmpty) {
        newWorkers
      } else {
        println("else")
        if(workers.head.id == id) {
          val d = new Date().getTime()/1000
          println(newWorkers)
          var C = newWorkers + new Worker(workers.head.id, workers.head.name, d.toInt)
          println(C)
          clockEmployee(workers.tail, C)
        } else {
          var D = newWorkers + workers.head
          clockEmployee(workers.tail, D)
        }
      }
    }
    workers = clockEmployee(workers, newWorkers)
  }
  
  /**
   * Given a workers id, updates the ClockOut attribute with the current time.
   */
  def clockOut(id: Long) {
    var newWorkers = Set.empty[Worker]
    def clockEmployee(workers: Set[Worker], newWorkers: Set[Worker]): Set[Worker] = {
      if (workers.isEmpty) {
        newWorkers
      } else {
        println("else")
        if(workers.head.id == id) {
          val d = new Date().getTime()/1000
          println(newWorkers)
          var C = newWorkers + new Worker(workers.head.id, workers.head.name, workers.head.clockIn, d.toInt)
          println(C)
          clockEmployee(workers.tail, C)
        } else {
          var D = newWorkers + workers.head
          clockEmployee(workers.tail, D)
        }
      }
    }
    workers = clockEmployee(workers, newWorkers)
  }
  
  /**
   * Given a workers id, compares that workers clock in and clock out times to see if they are currently at work or not. Returns true or false.
   */
  def isAtWork(id: Long): Boolean = {
    var worker : Worker = null
    for (a <- workers) {
      if (id == a.id) {
        worker = a
      }
    }
    if (worker.clockIn.>(worker.clockOut)) {
      true
    } else {
      false
    }
  }
}