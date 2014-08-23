package proofpeer.vue.components

/** The grid is a tool for bringing order to (web) design.
  * For information on grids, read '''Ordering Disorder: Grid Principles for Web Design''' by ''Khoi Vinh''.
  */ 
trait Grid {

  /** The number of units in the grid. Note that unlike Khoi Vinh, we use the term ''column'' to refer to 
    * the unit together with its left and right gutters. Units are numbered starting with 0.
    */
  def numUnits : Int

  /** The start coordinate of the unit. */
  def unitX(unit : Int) : Int

  /** The width of the unit. */
  def unitWidth(unit : Int) : Int

  /** The start coordinate of the left gutter of the unit. */
  def leftGutterX(unit : Int) : Int

  /** The width of the left gutter of the unit. */
  def leftGutterWidth(unit : Int) : Int

  /** The start coordinate of the right gutter of the unit. */
  def rightGutterX(unit : Int) : Int

  /** The width of the right gutter of the unit. */
  def rightGutterWidth(unit : Int) : Int

  /** The baseline height of the grid. */
  def baseline : Int
  
  /** The start coordinate of the column associated with the unit. */
  def columnX(unit : Int) : Int = leftGutterX(unit)
  
  /** The width of the column associated with the unit. */
  def columnWidth(unit : Int) : Int = 
    leftGutterWidth(unit) + unitWidth(unit) + rightGutterWidth(unit)
  
  /** Sums up the width of all columns associated with a unit such that unit1 <= unit <= unit2. */
  def columnsWidth(unit1 : Int, unit2 : Int) : Int = 
    leftGutterWidth(unit1) + unitsWidth(unit1, unit2) + rightGutterWidth(unit2)
  
  /** Same as columnsWidth(unit1, unit2) minus the left gutter width of unit1 and the right gutter width of unit2. */
  def unitsWidth(unit1 : Int, unit2 : Int) : Int = 
    rightGutterX(unit2) - unitX(unit1)
    
  /** The width of the grid. */
  def width : Int = columnsWidth(0, numUnits - 1)
    
  /** Creates a subgrid which starts at unit1 and ends at unit2 (and includes all the corresponding gutters).    
    * The coordinate system is shifted such that the top/left most point of the subgrid becomes (0,0).
    */
  def subgrid(unit1 : Int, unit2 : Int) : Grid = new Subgrid(this, unit1, unit2)
  
  def unitExtent(unit1 : Int, unit2 : Int, 
      includeLeftGutter : Boolean, includeRightGutter : Boolean) : (Int, Int) =
  {
    val x1 = if (includeLeftGutter) columnX(unit1) else unitX(unit1)
    val x2 = 
      if (includeRightGutter)
        columnX(unit2) + columnWidth(unit2)
      else
        unitX(unit2) + unitWidth(unit2)
    (x1, x2 - x1)
  }
  
}

class Subgrid(grid : Grid, unit1 : Int, unit2 : Int) extends Grid { 
  private val originX = grid.columnX(unit1)
  def numUnits : Int = unit2 - unit1 + 1
  def unitX(unit : Int) : Int = grid.unitX(unit1 + unit) - originX
  def unitWidth(unit : Int) : Int = grid.unitWidth(unit1 + unit)
  def leftGutterX(unit : Int) : Int = grid.leftGutterX(unit1 + unit) - originX
  def leftGutterWidth(unit : Int) : Int = grid.leftGutterWidth(unit1 + unit)
  def rightGutterX(unit : Int) : Int = grid.rightGutterX(unit1 + unit) - originX
  def rightGutterWidth(unit : Int) : Int = grid.rightGutterWidth(unit1 + unit)
  def baseline : Int = grid.baseline
  override def subgrid(u1 : Int, u2 : Int) = grid.subgrid(unit1 + u1, unit1 + u2)
}

trait GridStyle {
  def grid(width : Int, baseline : Int) : Grid 
}

/** The GoldenGridSystem is defined by its width, the number of its units, and the baseline height.
  * The units are all of equal width, and the width of its intra unit gutters is baseline/2 
  * (so the gap between two units is 2 * (baseline/2), while the outermost gutters are adapted to 
  * fit the overall width of the grid. 
  */
class GoldenGridSystem(override val width : Int, val numUnits : Int, val baseline : Int) extends Grid {  

  override val hashCode : Int = List(width, numUnits, baseline).hashCode

  override def equals(other : Any) = other match {
    case ggs : GoldenGridSystem => 
      width == ggs.width && numUnits == ggs.numUnits && baseline == ggs.baseline
    case _ => false
  }

  val gutter = baseline / 2 
  
  val (u, w) = {
      val u = width / numUnits - 2 * gutter
      if (u < 0) 
        (0, 0)
      else 
        (u, width - numUnits * (u + 2 * gutter))
    }
  val wleft = w / 2
  val wright = w - wleft
  val columnWidth = 2 * gutter + u
  val leftMostColumnWidth = wleft + columnWidth
    
  def leftGutterX(unit : Int) : Int = {
    unit match {
      case 0 => 0
      case _ => leftMostColumnWidth + (unit - 1) * columnWidth
    }
  }
  
  def leftGutterWidth(unit : Int) : Int = {
    unit match {
      case 0 => gutter + wleft
      case _ => gutter
    }
  } 
  
  def unitX(unit : Int) : Int = {
    unit match {
      case 0 => gutter + wleft
      case _ => leftMostColumnWidth + (unit - 1) * columnWidth + gutter
    }
  }
  
  def unitWidth(unit : Int) : Int = u
  
  def rightGutterX(unit : Int) : Int = {
    unit match {
      case 0 => gutter + wleft + u
      case _ => leftMostColumnWidth + (unit - 1) * columnWidth + gutter + u
    }
  }
  
  def rightGutterWidth(unit : Int) : Int = {
    if (unit == numUnits - 1) 
      gutter + wright
    else
      gutter
  }
  
}

import proofpeer.vue._
import proofpeer.vue.dom._

object GRID_LAYOUT extends CustomComponentClass {

  case class Position(startUnit : Int, endUnit : Int,
    includeLeftGutter : Boolean, includeRightGutter : Boolean,
    startBaseline : Coordinate, endBaseline : Coordinate)

  object GRID extends CustomAttributeName[Grid]("grid")
  object POSITIONS extends CustomAttributeName[List[Position]]("positions")
  object SHOW_GRID extends CustomAttributeName[Boolean]("showgrid")

  private def computeBaseline(dims : Dimensions, grid : Grid, baseline : Coordinate) : Int = {
    val numBaselines = dims.height / grid.baseline
    if (numBaselines <= 1) return 0
    var b = math.round((numBaselines - 1) * baseline.percentage).asInstanceOf[Int]
    if (b < 0) b = 0
    if (b >= numBaselines) b = numBaselines - 1
    b = b + baseline.offset
    if (b < 0) b = 0
    if (b >= numBaselines) b = numBaselines - 1
    return b
  }

  def render(c : CustomComponent) : Blueprint = {
    val grid = c.attributes(GRID)
    var positions : List[Position] = c.attributes(POSITIONS)
    val dims = c.attributes(DIMS)
    ensure(grid.width == dims.width, "grid has incompatible width")
    var children = c.children
    ensure(positions.size == children.size, "number of positions and children must match")
    var result : List[Blueprint] = List()
    val showgrid =
      c.attributes.get(SHOW_GRID) match {
        case None => false
        case Some(b) => b
      }
    if (showgrid) {
      positions = Position(0, grid.numUnits - 1, true, true, Percentage(0), Percentage(1)) :: positions
      children = SHOW_GRID_COMPONENT(GRID -> grid)() +: children
    }
    while (!positions.isEmpty) {
      val position = positions.head
      val child = children.head
      positions = positions.tail
      children = children.tail
      val (x, w) = grid.unitExtent(position.startUnit, position.endUnit,
        position.includeLeftGutter, position.includeRightGutter)
      val y1 = grid.baseline * computeBaseline(dims, grid, position.startBaseline)
      val y2 = grid.baseline * (1 + computeBaseline(dims, grid, position.endBaseline)) - 1
      val attrs = Dimensions(w, y2-y1+1, dims.pixelRatio).toAttributes(x, y1)
      result = (child + attrs) :: result
    }
    DIV(c)(result.reverse : _*)
  }

}

object SHOW_GRID_COMPONENT extends CustomComponentClass {

  val baseline_color = "red"
  val unit_color = "#D5EAF7"
  val gutter_color = "white"

  def rect(color : String, x : Int, y : Int, w : Int, h : Int) : Blueprint = {
    val attrs = Dimensions(w, h, 1).toAttributes(x, y)
    val child = DIV(STYLE->("background-color:"+color))()
    child + attrs
  }

  def render(c : CustomComponent) : Blueprint = {
    val grid = c.attributes(GRID_LAYOUT.GRID)
    val dims = c.attributes(DIMS)
    var children : List[Blueprint] = List()
    for (u <- 0 until grid.numUnits) {
      children = rect(unit_color, grid.unitX(u), 0, grid.unitWidth(u), dims.height) :: children
      children = rect(gutter_color, grid.leftGutterX(u), 0, grid.leftGutterWidth(u), dims.height) :: children
      children = rect(gutter_color, grid.rightGutterX(u), 0, grid.rightGutterWidth(u), dims.height) :: children
    }
    var b = grid.baseline
    while (b <= dims.height) {
      children = rect(baseline_color, 0, b-1, dims.width, 1) :: children
      b = b + grid.baseline
    }
    DIV(c)(children.reverse : _*)
  }

}

