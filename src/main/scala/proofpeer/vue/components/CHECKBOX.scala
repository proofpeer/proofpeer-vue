package proofpeer.vue.components

import proofpeer.vue._
import proofpeer.vue.dom._

object CHECKBOX extends CustomComponentClass {

  private val key = "checkbox"

  def render(parentNode : dom.Node, c : CustomComponent) : Blueprint = {
    val cs = ConfigSheet()
    val fontStyle = c.attributes.get(FONT_STYLE) match {
      case None => cs.bodyStyle
      case Some(fs) => fs
    }
    var style = "margin:0;appearance:none;outline:none;box-shadow:none;border-radius:none;border:0;"     
    val checkbox = INPUT(STYLE -> style, TYPE -> "checkbox")()
    val (checkboxWidth, checkboxHeight) = RenderTarget.measure(parentNode, checkbox)
    val marginTop = fontStyle.lineHeight - checkboxHeight
    style = style + "margin-top:" + marginTop + "px;"

    import SPAN_LAYOUT._

    val width = c.attributes.get(DIMS) match {
      case Some(dims) if dims.maximalWidth.isDefined =>
        var w = dims.maximalWidth.get - checkboxWidth - cs.gutterWidth
        if (w < 0) w = 0
        FIXED(w)
      case _ => AUTO
    }

    // render it
    SPAN_LAYOUT(c, WIDTHS -> List(FIXED(checkboxWidth), FIXED(cs.gutterWidth), width))(
      INPUT(STYLE->style, KEY->key, TYPE -> "checkbox")(),
      DIV()(),
      COPY(FONT_STYLE -> fontStyle)(
        c.children : _*
      )
    )
  }

  override def setState(component : CustomComponent, state : Any) { 
    val on : Boolean = state.asInstanceOf[Boolean]
    component(key).mountNode().checked = on
  }

  override def getState(component : CustomComponent) : Any = { 
    component(key).mountNode().checked.asInstanceOf[Boolean]
  }  

}