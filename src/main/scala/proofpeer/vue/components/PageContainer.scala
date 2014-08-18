package proofpeer.vue.components

import proofpeer.vue._
import DOM._
import AttributeName._

case class Dimensions(width : Int, height : Int, pixelRatio : Float)

object PageContainer extends CustomComponentClass {

  private def grabDimensions : Dimensions = {
    val pageWidth = window.innerWidth.asInstanceOf[Int]
    val pageHeight = window.innerHeight.asInstanceOf[Int]
    val pixelRatio = window.devicePixelRatio.asInstanceOf[Float]
    Dimensions(pageWidth, pageHeight, pixelRatio)
  }

  override def componentWillMount(component : CustomComponent) {
    component.setLocalState(grabDimensions)
    val f : () => Unit = () => {
      component.setLocalState(grabDimensions)
    }
    window.onresize = f
  }

  override def componentWillUnmount(component : CustomComponent) {
  }

  def render(component : CustomComponent) : Blueprint = {
    val dims = component.localState[Dimensions]
    val attrs = Attributes(STYLE -> ("overflow:hidden;position:absolute;left:0px;top:0px;width:"+dims.width+"px;height:"+dims.height+"px"))
    if (component.blueprint.children.isEmpty)
      DIV(attrs)(text("I am a page container: " + dims))
    else 
      DIV(attrs)(component.blueprint.children.head)
  }

}