package controllers

import play.api._
import play.api.mvc._
import play.api.Play.current
import play.api.mvc.{ Controller, Action}
import models.Order
import util.OrderStatus
import play.api.i18n.Messages
import play.api.i18n.Messages.Implicits._

class Dispatching extends Controller {

  def dispatchOrders = Action {
    Order.dispatch
    Redirect(routes.Orders.list())
  }
}