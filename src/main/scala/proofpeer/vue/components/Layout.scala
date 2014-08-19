package proofpeer.vue

import proofpeer.vue.dom.STYLE

object BOUNDS extends CustomAttributeName[Layout.Bounds]("bounds")

object Layout {
  case class Dimensions(width : Int, height : Int, pixelRatio : Float)

  case class Position(x : Int, y : Int)

  case class Bounds(position : Position, dimensions : Dimensions) {
    def x = position.x
    def y = position.y
    def width = dimensions.width
    def height = dimensions.height
    def pixelRation = dimensions.pixelRatio
    def toAttributes : Attributes = {
      Attributes(STYLE -> ("position:absolute;left:"+x+"px;top:"+y+"px;width:"+width+"px;height:"+height+"px"),
                 BOUNDS -> this)
    }
  }  
}