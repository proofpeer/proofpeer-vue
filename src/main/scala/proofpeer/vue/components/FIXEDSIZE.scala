package proofpeer.vue.components

import proofpeer.vue._
import dom._

object FIXEDSIZE extends CustomComponentClass {

  object X extends CustomAttributeName[Int]("x")
  object Y extends CustomAttributeName[Int]("y")  
  object DX extends CustomAttributeName[Int]("dx")
  object DY extends CustomAttributeName[Int]("dy")

  def render(parentNode : dom.Node, component : CustomComponent) : Blueprint = {
    val dims = component.attributes.get(DIMS) match {
        case Some(dims) => dims
        case None => Dimensions.unknown
      }
    val dx = component.attributes.get(DX) match {
        case Some(dx) => Some(dx)
        case None => dims.width
      }
    val dy = component.attributes.get(DY) match {
        case Some(dy) => Some(dy)
        case None => dims.height        
    }
    ensure(component.children.size == 1, "exactly 1 child expected")
    val child = component.children.head 
    var style = ""
    if (dx.isDefined) style += "width:" + dx.get + "px;"
    if (dy.isDefined) style += "height:" + dy.get + "px;"
    val x = component.attributes(X, 0)
    val y = component.attributes(Y, 0)
    val childAttrs = Dimensions.make(dx, dy, dims.pixelRatio).toAttributes +
      Dimensions.absoluteTopLeft(x, y)
    DIV(component, STYLE -> style)(
      child + childAttrs
    )    
  }

}
