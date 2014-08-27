package proofpeer.vue.components

import proofpeer.vue._
import dom._

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

object ALIGN extends CustomComponentClass {

  sealed trait VerticalAlignment 
  sealed trait HorizontalAlignment
  case object CENTER extends VerticalAlignment with HorizontalAlignment
  case object LEFT extends HorizontalAlignment
  case object RIGHT extends HorizontalAlignment
  case object TOP extends VerticalAlignment
  case object BOTTOM extends VerticalAlignment

  object V extends CustomAttributeName[VerticalAlignment]("valign")
  object H extends CustomAttributeName[HorizontalAlignment]("halign")

  private def alignmentOf(a : HorizontalAlignment) : Int = {
    a match {
      case CENTER => 0
      case LEFT => -1
      case RIGHT => 1
    }
  }

  private def alignmentOf(a : VerticalAlignment) : Int = {
    a match {
      case CENTER => 0
      case TOP => -1
      case BOTTOM => 1
    }
  }

  private def align(minimal : Int, max : Option[Int], preferred : Int, alignment : Int) : (Int, Int) = {
    if (preferred <= minimal) {
      if (alignment < 0) 
        (minimal, 0)
      else if (alignment > 0)
        (minimal, minimal - preferred)
      else 
        (minimal, (minimal - preferred) / 2)
    } else {
      if (max.isDefined) {
        val maximal = max.get
        if (preferred <= maximal)
          (preferred, 0)
        else {
          if (alignment < 0)
            (maximal, 0)
          else if (alignment > 0)
            (maximal, maximal - preferred)
          else 
            (maximal, (maximal - preferred) / 2)
        }
      } else {
        (preferred, 0)
      } 
    }
  }

  def render(parentNode : dom.Node, component : CustomComponent) : Blueprint = {
    val dims = component.attributes.get(DIMS) match {
        case Some(dims) => dims
        case None => Dimensions.unknown
      }
    val valign = component.attributes.get(V) match {
        case None => CENTER
        case Some(v) => v
      }
    val halign = component.attributes.get(H) match {
        case None => CENTER
        case Some(h) => h
    }
    ensure(component.children.size == 1, "exactly 1 child expected")
    val child = component.children.head 
    val (childWidth, childHeight) = RenderTarget.measure(parentNode, child + (DIMS -> dims.upperBounds))
    val (w, x) = align(dims.minimalWidth, dims.maximalWidth, childWidth, alignmentOf(halign))
    val (h, y) = align(dims.minimalHeight, dims.maximalHeight, childHeight, alignmentOf(valign))    
    val childAttrs = Dimensions.make(childWidth, childHeight, dims.pixelRatio).toAttributes +
      Dimensions.absoluteTopLeft(x, y)
    DIV(component, STYLE -> ("width:"+w+"px;height:"+h+"px;overflow:hidden"))(
      child + childAttrs
    )    
  }

}


