package controllers

import models.Product
import models.ProductPart
import play.api._
import play.api.data.Form
import play.api.data.Forms.{mapping, longNumber, nonEmptyText}
import play.api.i18n.Messages
import play.api.i18n.Messages.Implicits._
import play.api.mvc._
import play.api.mvc.{Action, Controller, Flash}
import play.api.Play.current
import models.Shelf

class Products extends Controller {
  private val productForm: Form[ProductPart] = Form(mapping("ean" -> longNumber , "quantity" -> longNumber, "location" -> nonEmptyText)(ProductPart.apply)(ProductPart.unapply))
  
  def list = Action {
    implicit request =>
      val products = Product.findAll
      Ok(views.html.list(products))
  }
  
  def show(ean: Long) = Action {
    implicit request =>
      Product.findByEan(ean).map {
        product => Ok(views.html.details(product))
      }.getOrElse(NotFound)
  }
  
  def damaged = Action {
    implicit request =>
      val damagedProductForm = productForm.bindFromRequest()
      damagedProductForm.fold(
        hasErrors = {
          form => Redirect(routes.Products.damagedProduct()).flashing(Flash(form.data) + ("error" -> Messages("validation.errors")))
        },
        success = {
          damagedProduct => Product.damaged(damagedProduct)
          Shelf.removeFromShelf(damagedProduct.location, damagedProduct.quantity)
          val message = Messages("products.damaged.success", damagedProduct.ean)
          Redirect(routes.Products.show(damagedProduct.ean)).flashing("success" -> message)
        }
      )
  }
  
  def damagedProduct = Action {
    implicit request =>
      val form = if(request2flash.get("error").isDefined)
          productForm.bind(request2flash.data)
        else
          productForm
      Ok(views.html.damagedProduct(form))
  }
}