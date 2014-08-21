package proofpeer.vue.components

import proofpeer.vue._
import proofpeer.vue.dom._


object RASTER_LAYOUT extends CustomComponentClass {

  object POSITIONS extends CustomAttributeName[List[Position]]("positions")

  case class Position(x1 : Coordinate, y1 : Coordinate, 
    x2 : Coordinate, y2 : Coordinate)

  private def rasterize(d : Int, c : Coordinate) : Int = {
    var i = math.round(d * c.percentage).asInstanceOf[Int]
    if (i >= d) i = d - 1
    if (i < 0) i = 0
    i = i + c.offset
    if (i >= d) i = d - 1
    if (i < 0) i = 0
    return i
  }
  
  def render(c : CustomComponent) : Blueprint = {
    var positions = c.attributes(POSITIONS)
    var children = c.children
    val dims = c.attributes(DIMS)
    ensure(positions.size == children.size, "number of positions and children must match")
    var result : List[Blueprint] = List()
    while (!positions.isEmpty) {
      val position = positions.head
      val child = children.head
      positions = positions.tail
      children = children.tail
      val x1 = rasterize(dims.width, position.x1)
      val x2 = rasterize(dims.width, position.x2)
      val y1 = rasterize(dims.height, position.y1)
      val y2 = rasterize(dims.height, position.y2)
      val attrs = Dimensions(x2-x1+1, y2-y1+1, dims.pixelRatio).toAttributes(x1, y1)
      result = (child + attrs) :: result
    }
    DIV(c)(result.reverse : _*)
  }

}


