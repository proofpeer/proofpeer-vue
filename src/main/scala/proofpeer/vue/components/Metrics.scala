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
  def upperBounds : Dimensions = {
    Dimensions(None, None, pixelRatio, None, maximalWidth, None, maximalHeight)
  }
  def minimalWidth : Int = {
    (width match {
      case None => min_width
      case _ => width
    }) match {
      case None => 0
      case Some(width) => width
    }
  }
  def minimalHeight : Int = {
    (height match {
      case None => min_height
      case _ => height
    }) match {
      case None => 0
      case Some(height) => height
    }
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
    Dimensions(Some(width), Some(height), Some(pixelRatio), None, None, None, None)
  }

  def make(width : Int, height : Int, pixelRatio : Option[Double]) : Dimensions = {
    Dimensions(Some(width), Some(height), pixelRatio, None, None, None, None)
  }

  val unknown : Dimensions = Dimensions(None, None, None, None, None, None, None)

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
