package proofpeer.vue.components

import proofpeer.vue._
import dom._
import scala.scalajs.js

object SINGLE_PAGE_APP extends CustomComponentClass {

  case class State(location : String, dims : Dimensions)

  private def splitLocation(location : String) : (String, String) = {
    location.lastIndexOf("#") match {
      case -1 => (location, "")
      case i => (location.substring(0, i), location.substring(i+1))
    }       
  }

  private def initRouting(c : CustomComponent) : String = {
    val f : (js.Dynamic) => Unit = event => {
      val location = document().location.href.asInstanceOf[String]
      c.setLocalState(State(location, grabDimensions))
    }
    window().onpopstate = f
    document().location.href.asInstanceOf[String]
  }

  private def grabDimensions : Dimensions = {
    val pageWidth = window().innerWidth.asInstanceOf[Int]
    val pageHeight = window().innerHeight.asInstanceOf[Int]
    val pixelRatio = window().devicePixelRatio.asInstanceOf[Double]
    Dimensions.make(pageWidth, pageHeight, pixelRatio)
  }

  override def componentWillMount(component : CustomComponent) {
    val location = initRouting(component)
    component.setLocalState(State(location, grabDimensions))
    val f : () => Unit = () => {
      component.getLocalState() match {
        case State(l, d) => component.setLocalState(State(l, grabDimensions))
      }
    }
    window().onresize = f
  }

  override def componentWillUnmount(component : CustomComponent) {
  }

  private def selectChild(location : String, children : Seq[Blueprint]) : Blueprint = {
    val (_, hash) = splitLocation(location)
    if (hash.isEmpty) children.head 
    else {
      for (child <- children) {
        child.attributes.get(KEY) match {
          case None =>
          case Some(key) => if ("page!" + key == hash) return child
        }
      }
      children.head
    }
  }

  def render(parentNode : dom.Node, component : CustomComponent) : Blueprint = {
    val state = component.localState[State]
    val dims = state.dims
    val childDims = Dimensions(dims.width, dims.height, dims.pixelRatio, None, None, None, None)
    ensure(component.blueprint.children.size > 0, "SINGLE_PAGE_APP expects at least one child")
    val child = selectChild(state.location, component.blueprint.children)
    val atZero = Dimensions.absoluteTopLeft(0,0)
    val attrs = dims.toAttributes + atZero
    DIV(attrs + (STYLE -> "overflow:hidden"))(child + childDims.toAttributes + atZero)
  }  

}