package proofpeer.vue.components

import proofpeer.vue._
import proofpeer.vue.dom._

object SPAN_LAYOUT extends CustomComponentClass {

  sealed trait WIDTH
  case object AUTO extends WIDTH
  case class FIXED(width : Int) extends WIDTH

  case object WIDTHS extends CustomAttributeName[List[WIDTH]]("widths")

  def render(parentNode : dom.Node, c : CustomComponent) : Blueprint = {
    var widths : List[Int] = List()
    var total_width = 0
    var children = c.children
    var positioned_children : List[Blueprint] = List()
    val dims = c.attributes(DIMS)
    for (w <- c.attributes(WIDTHS)) {
      val child = children.head
      val width = 
        w match {
          case FIXED(width) => width
          case AUTO =>
            val childDims = Dimensions(None, dims.height, dims.pixelRatio, None, None,
              dims.min_height, dims.max_height)
            val (width, _) = RenderTarget.measure(parentNode, child + childDims.toAttributes)
            width
        }
      val childDims = Dimensions(Some(width), dims.height, dims.pixelRatio, None, None,
          dims.min_height, dims.max_height)
      val attrs = childDims.toAttributes + Dimensions.absoluteTopLeft(total_width, 0)
      positioned_children = (child + attrs) :: positioned_children
      total_width = total_width + width
      children = children.tail
    }
    DIV(c, STYLE -> ("width:"+total_width+"px;overflow:visible"))(
      positioned_children.reverse : _*
    )
  }

}