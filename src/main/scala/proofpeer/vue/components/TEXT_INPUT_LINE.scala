package proofpeer.vue.components

import proofpeer.vue._
import proofpeer.vue.dom._

object TEXT_INPUT_LINE extends CustomComponentClass {

  sealed trait KIND
  case object PLAINTEXT extends KIND
  case object PASSWORD extends KIND

  object KIND extends CustomAttributeName[KIND]("kind")

  private def kindOf(c : CustomComponent) : KIND = {
    c.attributes.get(KIND) match {
      case None => PLAINTEXT
      case Some(kind) => kind
    }
  }

  def render(parentNode : dom.Node, c : CustomComponent) : Blueprint = {
    val cs = ConfigSheet()
    val fontStyle = c.attributes.get(FONT_STYLE) match {
      case None => cs.bodyStyle
      case Some(fs) => fs
    }
    val borderColor = "#c9c9c9"
    val backgroundColor = "#f0f0f0"
    val padding = 4
    val border = 2
    val dims = c.attributes(DIMS)
    var width = 7 * cs.gutterWidth
    if (dims.minimalWidth > width) width = dims.minimalWidth
    dims.maximalWidth match {
      case Some(w) => width = w
      case _ => 
    }
    val ty =
      kindOf(c) match {
        case PLAINTEXT => "text"
        case PASSWORD => "password"
      }

    // compute the style attribute
    val style = fontStyle + 
      "margin:0;appearance:none;outline:none;box-shadow:none;border-radius:none;border:0;" +
      "padding-left:"+padding+"px;padding-right:"+padding+"px;"+
      "border:solid "+border+"px "+borderColor+";"+
      "padding-top:"+(fontStyle.paddingTop - border)+"px;"+
      "background-color:"+backgroundColor+";"+
      "width:"+(width - 2*padding - 2*border)+"px;"+
      "height:"+(fontStyle.lineHeight)+"px;"
    
    // render it
    INPUT(c, STYLE -> style, TYPE -> ty)()
  }

}
