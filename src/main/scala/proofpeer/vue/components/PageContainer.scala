package proofpeer.vue.components

import proofpeer.vue._
import dom._
import Layout._

object PageContainer extends CustomComponentClass {

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
    val attrs = Attributes(STYLE -> ("overflow:hidden;position:absolute;left:0px;top:0px;width:"+dims.width+"px;height:"+dims.height+"px"))
    ensure(component.blueprint.children.size == 1, "PageContainer expects exactly one child")
    val child = component.blueprint.children.head
    val bounds = Bounds(Position(0, 0), dims).toAttributes
    DIV(bounds + (STYLE -> "overflow:hidden"))(child ++ bounds)
  }

}
