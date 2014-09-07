package proofpeer.vue.components

import proofpeer.vue._
import proofpeer.vue.dom._

object PUSH_BUTTON extends CustomComponentClass {

  private case object ON_MOUSE_OVER extends StringAttributeName("onMouseOver")
  private case object ON_MOUSE_OUT extends StringAttributeName("onMouseOut")

  def render(parentNode : Node, component : CustomComponent) : Blueprint = {
    val cs = ConfigSheet()
    val fontStyle = component.attributes(FONT_STYLE, cs.bodyStyle)
    val hoverBackgroundColor = "#006600"
    val backgroundColor = "#009900"
    val textColor = "white"
    val padding = 4
    val style = fontStyle + 
      "margin:0;appearance:none;outline:none;box-shadow:none;border-radius:none;border:0;" +
      "padding-left:"+padding+"px;padding-right:"+padding+"px;"+
      "background-color:"+backgroundColor+";color:"+textColor+";"
    val mouse_over = "this.style.backgroundColor='"+hoverBackgroundColor+"'"
    val mouse_out = "this.style.backgroundColor='" + backgroundColor +"'"
    BUTTON(component, STYLE -> style, ON_MOUSE_OVER -> mouse_over, ON_MOUSE_OUT -> mouse_out)(
      component.children : _*
    )
  }
}