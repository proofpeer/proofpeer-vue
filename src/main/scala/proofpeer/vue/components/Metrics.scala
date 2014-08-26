package proofpeer.vue.components

import proofpeer.vue.dom.STYLE
import proofpeer.vue._

case class Dimensions(
  width : Option[Int],
  height : Option[Int],
  pixelRatio : Option[Double],
  min_width : Option[Int],
  max_width : Option[Int],
  min_height : Option[Int],
  max_height : Option[Int]) 
{
  def toAttributes(x : Int, y : Int) : Attributes = {
    Dimensions.absolutePosition(x, y, width, height) +
      (DIMS -> this)
  }
  def upperBound : Dimensions = {
    Dimensions(None, None, pixelRatio, None, maximalWidth, None, maximalHeight)
  }
  def maximalWidth : Option[Int] = {
    width match {
      case None => max_width
      case _ => width
    }
  }
  def maximalHeight : Option[Int] = {
    height match {
      case None => max_height
      case _ => height
    }
  }
}

object Dimensions {

  def absolutePosition(x : Int, y : Int, width : Option[Int], height : Option[Int]) : Attributes = {
    val w = width match { 
      case None => "" 
      case Some(w) => "width:"+w+"px;" 
    }
    val h = height match {
      case None => ""
      case Some(h) => "height:"+h+"px;"
    }
    Attributes(STYLE -> 
      ("position:absolute;left:"+x+"px;top:"+y+"px;"+w+h))
  }

  def make(width : Int, height : Int, pixelRatio : Double) : Dimensions = {
    val w = Some(width)
    val h = Some(height)
    Dimensions(w, h, Some(pixelRatio), w, w, h, h)
  }

  def make(width : Int, height : Int, pixelRatio : Option[Double]) : Dimensions = {
    val w = Some(width)
    val h = Some(height)
    Dimensions(w, h, pixelRatio, w, w, h, h)
  }


}

case object DIMS extends CustomAttributeName[Dimensions]("dims")

/** A coordinate consists of both a percentage part (which must be between 0 and 1) and an offset part. */
case class Coordinate(percentage : Double, offset : Int)

object Offset {
  def apply(c : Int) : Coordinate = Coordinate(0, c)
}

object Percentage {
  def apply(c : Double) : Coordinate = Coordinate(c, 0)
}
