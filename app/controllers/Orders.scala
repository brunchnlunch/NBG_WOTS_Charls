package controllers

import play.api._
import play.api.mvc._
import play.api.Play.current
import play.api.mvc.{ Controller, Action, Flash }
import models.Order
import util.OrderStatus
import play.api.i18n.Messages
import play.api.i18n.Messages.Implicits._
import models.Packaging
import models.Shelf

class Orders extends Controller {

  def list = Action {
    implicit request =>
      val orders = Order.findAll
      Ok(views.html.orderlist(orders))
  }

  def show(id: Long) = Action {
    implicit request =>
      Order.findById(id).map {
        order => Ok(views.html.orderDetails(order))
      }.getOrElse(NotFound)
  }

  def getOrder = Action {
    implicit request =>
      val order = getAnOrder
      val locations = Shelf.findForOrder(order)
      Ok(views.html.pickorder(order, locations))
  }

  def getAnOrder: Order = {
    if (Order.findNextOrder().==(null)) {
      println("No Order Yet")
      Thread.sleep(100000)
      getAnOrder
    } else {
      val order = Order.findNextOrder()
      Order.updateStatus(order.id, Order.premoteStatus(_))
      Order.findById(order.id).head
    }
  }

  def changeStatus(id: Long, update: Boolean) = Action {
    implicit request =>
    if(!update) {
      Order.updateStatus(id, Order.demoteStatus(_))
      Redirect(routes.Orders.list())
    } else {
      Order.updateStatus(id, Order.premoteStatus(_))
      if(Order.findById(id).get.status.equals(OrderStatus.PICKED)) {
          val boxes = Packaging.selectBox(Order.calcVolume(Order.findById(id).get))
          val order = Order.findById(id).get
          Ok(views.html.packorder(order, boxes))
      } else {
        Redirect(routes.Orders.list())
      }
    }
  }
}