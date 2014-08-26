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

object CENTERED extends CustomComponentClass {

  def render(parentNode : dom.Node, component : CustomComponent) : Blueprint = {
    DIV(component)(
      DIV(STYLE -> "margin:auto;position:absolute;top:0;left:0;bottom:0;right:0;height:0px")(
        DIV(STYLE -> "display:table;margin:0 auto")(
          component.children : _*
        )
      )
    )
  }

}

object CENTER extends CustomComponentClass {

  def render(parentNode : dom.Node, component : CustomComponent) : Blueprint = {
    val dims = component.attributes(DIMS)
    val w = dims.width.get
    val h = dims.height.get
    ensure(component.children.size == 1, "exactly 1 child expected")
    val child = component.children.head 
    val (childWidth, childHeight) = RenderTarget.measure(parentNode, child + (DIMS -> dims.upperBound))
    val x = (w-childWidth) / 2
    val y = (h-childHeight) / 2
    val childAttrs = Dimensions.make(childWidth, childHeight, dims.pixelRatio).toAttributes(x, y)
    DIV(component)(
      child + childAttrs
    )
  }  
}


