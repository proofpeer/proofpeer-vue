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
    def asStyle(hover : Boolean) : String = {
      val bgColor = if (hover) hoverBackgroundColor else backgroundColor
      "cursor:pointer;margin:0;appearance:none;outline:none;box-shadow:none;border-radius:none;" +
      "border:solid "+border+"px "+borderColor+";text-decoration:none;"+
      "background-color:"+bgColor +";color:"+textColor+";"
    }
    def cssClass : String
    def register() {
      val style = asStyle(false)
      val normalRule = "." + cssClass + "{" + style + "}"
      val linkRule = "." + cssClass + ":link{" + style + "}"
      val visitedRule = "." + cssClass + ":visited{" + style + "}"
      val hoverRule = "." + cssClass + ":hover{" + asStyle(true) + "}"
      Styling.addRules(normalRule, linkRule, visitedRule, hoverRule)
    }
  }

  object IMPORTANT extends UrgencyLevel {
    val hoverBackgroundColor = "#006600"
    val backgroundColor = "#009900"
    val textColor = "white"
    val border = 0
    val borderColor = "white"
    val cssClass = "PUSH-BUTTON-IMPORTANT"
    register()
  }

  object DEFAULT extends UrgencyLevel {
    val hoverBackgroundColor = "#C9C9C9" 
    val backgroundColor = "white"
    val textColor = "black"
    val border = 2
    val borderColor = "#C9C9C9"
    val cssClass = "PUSH-BUTTON-DEFAULT"
    register()
  }

  object WARNING extends UrgencyLevel {
    val hoverBackgroundColor = "#FF3300"
    val backgroundColor = "#FF6600"
    val textColor = "white"
    val border = 0
    val borderColor = "white"
    val cssClass = "PUSH-BUTTON-WARNING"
    register()
  }

  object URGENCY extends CustomAttributeName[UrgencyLevel]("urgency")
  object LINKTO extends CustomAttributeName[String]("linkto")

  def render(parentNode : Node, component : CustomComponent) : Blueprint = {
    val cs = ConfigSheet()
    val fontStyle = component.attributes(FONT_STYLE, cs.bodyStyle)
    val urgency = component.attributes(URGENCY, DEFAULT)
    val padding = 4
    val style = fontStyle + 
      "padding-left:"+padding+"px;padding-right:"+padding+"px;"+
      "padding-top:"+(fontStyle.paddingTop - urgency.border)+"px;"+
      "padding-bottom:"+(padding - urgency.border)+"px;"

    component.attributes.get(LINKTO) match {
      case None =>
        BUTTON(component, STYLE -> style, CLASS -> urgency.cssClass)(
          component.children : _*
        )      
      case Some(location) =>
        val linkstyle = "vertical-align:middle;text-align:center;" + fontStyle
        LINK(component, CLASS -> urgency.cssClass, HREF -> location)(
          DIV(STYLE->linkstyle)(component.children : _*)
        )      
    }  
  }
}