package proofpeer.vue.components

import proofpeer.vue._
import proofpeer.vue.dom._

object TEXT_INPUT_AREA extends CustomComponentClass {

  private val key = "textarea"

  def render(parentNode : dom.Node, c : CustomComponent) : Blueprint = {
    val cs = ConfigSheet()
    val fontStyle = c.attributes(FONT_STYLE, cs.bodyStyle)
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

    val rows = c.attributes(ROWS, 3)

    // compute the style attribute
    val style = fontStyle + 
      "margin:0;appearance:none;outline:none;box-shadow:none;border-radius:none;" +
      "padding-left:"+padding+"px;padding-right:"+padding+"px;"+
      "border:solid "+border+"px "+borderColor+";"+
      "padding-top:"+(fontStyle.paddingTop - border)+"px;"+
      "background-color:"+backgroundColor+";"+
      "width:"+(width - 2*padding - 2*border)+"px;"+
      "height:"+(fontStyle.lineHeight * rows)+"px;"+
      "resize:none;"
    
    // render it
    TEXTAREA(c, KEY -> key, STYLE -> style, SPELLCHECK -> false)()
  }

  override def setState(component : CustomComponent, state : Any) { 
    component(key).setState(state) 
  }

  override def getState(component : CustomComponent) : Any = { 
    component(key).getState
  }


}