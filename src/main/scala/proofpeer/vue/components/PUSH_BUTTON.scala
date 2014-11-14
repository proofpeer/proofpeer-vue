package proofpeer.vue.components

import proofpeer.vue._
import proofpeer.vue.dom._

object PUSH_BUTTON extends CustomComponentClass {

  private case object ON_MOUSE_OVER extends StringAttributeName("onMouseOver")
  private case object ON_MOUSE_OUT extends StringAttributeName("onMouseOut")

  sealed trait UrgencyLevel {
    def hoverBackgroundColor : String
    def backgroundColor : String
    def textColor : String
    def border : Int 
    def borderColor : String
  }

  object IMPORTANT extends UrgencyLevel {
    val hoverBackgroundColor = "#006600"
    val backgroundColor = "#009900"
    val textColor = "white"
    val border = 0
    val borderColor = "white"
  }

  object DEFAULT extends UrgencyLevel {
    val hoverBackgroundColor = "#C9C9C9" 
    val backgroundColor = "white"
    val textColor = "black"
    val border = 2
    val borderColor = "#C9C9C9"
  }

  object WARNING extends UrgencyLevel {
    val hoverBackgroundColor = "#FF3300"
    val backgroundColor = "#FF6600"
    val textColor = "white"
    val border = 0
    val borderColor = "white"
  }

  object URGENCY extends CustomAttributeName[UrgencyLevel]("urgency")

  def render(parentNode : Node, component : CustomComponent) : Blueprint = {
    val cs = ConfigSheet()
    val fontStyle = component.attributes(FONT_STYLE, cs.bodyStyle)
    val urgency = component.attributes(URGENCY, DEFAULT)
    val hoverBackgroundColor = urgency.hoverBackgroundColor
    val backgroundColor = urgency.backgroundColor
    val textColor = urgency.textColor
    val padding = 4
    val style = fontStyle + 
      "margin:0;appearance:none;outline:none;box-shadow:none;border-radius:none;" +
      "padding-left:"+padding+"px;padding-right:"+padding+"px;"+
      "background-color:"+backgroundColor+";color:"+textColor+";"+
      "border:solid "+urgency.border+"px "+urgency.borderColor+";"+
      "padding-top:"+(fontStyle.paddingTop - urgency.border)+"px;"+
      "padding-bottom:"+(padding - urgency.border)+"px;"
      //"height:"+(fontStyle.lineHeight + fontStyle.paddingTop + urgency.border)+"px;"
    val mouse_over = "this.style.backgroundColor='"+hoverBackgroundColor+"'"
    val mouse_out = "this.style.backgroundColor='" + backgroundColor +"'"
    BUTTON(component, STYLE -> style, ON_MOUSE_OVER -> mouse_over, ON_MOUSE_OUT -> mouse_out)(
      component.children : _*
    )
  }
}