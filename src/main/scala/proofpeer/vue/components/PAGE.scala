package proofpeer.vue.components

import proofpeer.vue._
import dom._
import Layout._

object PAGE extends CustomComponentClass {

  private def grabDimensions : Dimensions = {
    val pageWidth = window().innerWidth.asInstanceOf[Int]
    val pageHeight = window().innerHeight.asInstanceOf[Int]
    val pixelRatio = window().devicePixelRatio.asInstanceOf[Float]
    Dimensions(pageWidth, pageHeight, pixelRatio)
  }

  override def componentWillMount(component : CustomComponent) {
    component.setLocalState(grabDimensions)
    val f : () => Unit = () => {
      component.setLocalState(grabDimensions)
    }
    window().onresize = f
  }

  override def componentWillUnmount(component : CustomComponent) {
  }

  def render(component : CustomComponent) : Blueprint = {
    val dims = component.localState[Dimensions]
    ensure(component.blueprint.children.size == 1, "PageContainer expects exactly one child")
    val child = component.blueprint.children.head
    val bounds = Bounds(Position(0, 0), dims).toAttributes
    DIV(bounds + (STYLE -> "overflow:hidden"))(child + bounds)
  }

}

object SHOW_BOUNDS extends CustomComponentClass {

  def render(component : CustomComponent) : Blueprint = {
    val t = 
      component.attributes.get(BOUNDS) match {
        case None => "no bounds found"
        case Some(bounds) => 
          "("+bounds.x+", "+bounds.y+"), "+bounds.width+"x"+bounds.height+
          ", pixelRatio="+bounds.pixelRatio
      }
    CENTERED(component.attributes * (STYLE -> "background-color:black;color:white"))(text(t))
  }

}

object CENTERED extends CustomComponentClass {

  def render(component : CustomComponent) : Blueprint = {
    DIV(component)(
      DIV(STYLE -> "margin:auto;position:absolute;top:0;left:0;bottom:0;right:0;height:0px")(
        DIV(STYLE -> "display:table;margin:0 auto")(
          component.children : _*
        )
      )
    )
  }

}
