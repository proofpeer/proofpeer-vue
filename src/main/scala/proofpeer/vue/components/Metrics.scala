package proofpeer.vue.components

import proofpeer.vue.dom.STYLE
import proofpeer.vue._

case object DIMS extends CustomAttributeName[Dimensions]("dimensions")

case class Dimensions(width : Int, height : Int, pixelRatio : Double) {
  def toAttributes(left : Int, top : Int) : Attributes = {
    Attributes(STYLE -> ("position:absolute;left:"+left+"px;top:"+top+"px;width:"+width+"px;height:"+height+"px"),
               DIMS -> this)  
  }
}

/** A coordinate consists of both a percentage part (which must be between 0 and 1) and an offset part. */
case class Coordinate(percentage : Double, offset : Int)

object Offset {
  def apply(c : Int) : Coordinate = Coordinate(0, c)
}

object Percentage {
  def apply(c : Double) : Coordinate = Coordinate(c, 0)
}
