package proofpeer.vue.components

import proofpeer.vue._
import dom._

object PAGE extends CustomComponentClass {

  private def grabDimensions : Dimensions = {
    val pageWidth = window().innerWidth.asInstanceOf[Int]
    val pageHeight = window().innerHeight.asInstanceOf[Int]
    val pixelRatio = window().devicePixelRatio.asInstanceOf[Double]
    Dimensions.make(pageWidth, pageHeight, pixelRatio)
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

  def render(parentNode : dom.Node, component : CustomComponent) : Blueprint = {
    val dims = component.localState[Dimensions]
    ensure(component.blueprint.children.size == 1, "PageContainer expects exactly one child")
    val child = component.blueprint.children.head
    val attrs = dims.toAttributes(0, 0)
    DIV(attrs + (STYLE -> "overflow:hidden"))(child + attrs)
  }

}

object SHOW_DIMS extends CustomComponentClass {

  def render(parentNode : dom.Node, component : CustomComponent) : Blueprint = {
    val t = component.attributes.get(DIMS) match {
      case None => "no dimensions found" 
      case Some(dims) =>
        val pixelRatio =
          dims.pixelRatio match {
            case None => "@?"
            case Some(pixelRatio) => 
              val ratio = math.round(pixelRatio * 100) / 100.0
              if (ratio == 1)
                ""
              else
                "@"+ratio
          }
        val w = 
          dims.width match {
            case None => "?"
            case Some(w) => ""+w 
          }
        val h = 
          dims.height match {
            case None => "?"
            case Some(h) => ""+h 
          }
        w + "x" + h + pixelRatio
    }
    CENTERED(component.attributes + (STYLE -> "background-color:black;color:white;opacity:0.4"))(text(t))
  }

}

