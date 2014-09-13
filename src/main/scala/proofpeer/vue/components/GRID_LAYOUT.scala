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
class GoldenGridSystem(_width : Int, val numUnits : Int, val minUnitWidth : Int = 0) extends Grid {  

  val baseline = ConfigSheet().baselineHeight
  val gutter = ConfigSheet().gutterWidth
  
  val (u, w) = {
      val u = _width / numUnits - 2 * gutter
      if (u < minUnitWidth) 
        (minUnitWidth, 0)
      else 
        (u, _width - numUnits * (u + 2 * gutter))
    }
  val wleft = w / 2
  val wright = w - wleft
  val columnWidth = 2 * gutter + u
  val leftMostColumnWidth = wleft + columnWidth
  override val width = wleft + numUnits * (u + 2 * gutter) + wright
    
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

  override val hashCode : Int = List(width, numUnits, baseline).hashCode

  override def equals(other : Any) = other match {
    case ggs : GoldenGridSystem => 
      width == ggs.width && numUnits == ggs.numUnits && baseline == ggs.baseline
    case _ => false
  }  
  
}

import proofpeer.vue._
import proofpeer.vue.dom._

object GRID_LAYOUT extends CustomComponentClass {

  // This refers to either the start or end baseline of the component
  // which comes componentIndexDelta before (componentIndexDelta < 0) or after (componentIndexDelta > 0)
  // the current component. You can also refer to the start or end baseline of the 
  // current component (componentIndexDelta = 0).
  case class BaselineReference(componentIndexDelta : Int, start : Boolean)

  trait BaselineComputation {
    def apply(referencedBaselines : Array[Int]) : Int
  }

  case class Offset(offset : Int) extends BaselineComputation {
    def apply(referencedBaselines : Array[Int]) : Int = {
      ensure(referencedBaselines.size == 1, "Offset expects exactly one referenced baseline")
      referencedBaselines(0) + offset
    }
  }

  case class MaxBaseline(offset : Int) extends BaselineComputation {
    def apply(baselines : Array[Int]) : Int = {
      var maxBaseline = 0
      for (b <- baselines) {
        if (b > maxBaseline) maxBaseline = b
      }
      maxBaseline + offset
    }
  }

  sealed trait Baseline 

  // This is an absolute reference to a specific baseline.
  case class Absolute(baseline : Int) extends Baseline 

  // This refers to either the start or end baseline occupied by the component componentDelta 
  // components prior to the the current component, with an offset added to that baseline.   
  case class Relative(references : List[BaselineReference], computation : BaselineComputation) extends Baseline 

  
  sealed trait AutomaticMode
  case object CUT extends AutomaticMode
  case object FILL extends AutomaticMode

  case class Automatic(mode : AutomaticMode) extends Baseline 
  
  private def isAutomatic(baseline : Baseline) : Boolean = {
    baseline match {
      case Automatic(_) => true
      case _ => false
    }
  }

  def relative(componentDelta : Int, start : Boolean, offset : Int) : Baseline = {
    Relative(List(BaselineReference(componentDelta, start)), Offset(offset))
  }

  def belowPrevious(offset : Int) : Baseline = {
    relative(-1, false, offset)
  }

  def belowN(n : Int, offset : Int) : Baseline = {
    val references = (-n to -1).map(i => BaselineReference(i, false)).toList
    Relative(references, MaxBaseline(offset))
  }
  
  def heightBelow(height : Int) : Baseline = {
    relative(0, true, height-1)
  }

  def heightAbove(height : Int) : Baseline = {
    relative(0, false, -(height-1))
  }

  case class Position(startUnit : Int, endUnit : Int,
    includeLeftGutter : Boolean, includeRightGutter : Boolean,
    startBaseline : Baseline, endBaseline : Baseline)

  case object GRID extends CustomAttributeName[Grid]("grid")
  case object POSITIONS extends CustomAttributeName[List[Position]]("positions")
  case object SHOW_GRID extends CustomAttributeName[Boolean]("showgrid")
  case object IS_FORM extends CustomAttributeName[Boolean]("isform")

  private type Node = (Int, Boolean)

  private def computeComponentOrder(positions : Seq[Position]) : Seq[Node] = {
    var graph : Map[Node, Set[Node]] = Map()
    def addNode(node : Node) {
      if (!graph.get(node).isDefined)
        graph = graph + (node -> Set())
    }
    def addDependency(node : Node, dependency : Node) {
      graph.get(node) match {
        case None => 
          graph = graph + (node -> Set(dependency))
        case Some(oldDependencies) =>
          graph = graph + (node -> (oldDependencies + dependency))
      }
    }
    def addPosition(index : Int, position : Position) {
      val startBaseline = (index, true)
      val endBaseline = (index, false)
      position.startBaseline match {
        case Automatic(_) =>
          addDependency(startBaseline, endBaseline)
        case Absolute(_) => 
          addNode(startBaseline)
        case Relative(references, _) => 
          for (r <- references) {
            addDependency(startBaseline, (index + r.componentIndexDelta, r.start))
          }
      }
      position.endBaseline match {
        case Automatic(_) =>
          addDependency(endBaseline, startBaseline)
        case Absolute(_) =>
          addNode(endBaseline)
        case Relative(references, _) =>
          for (r <- references) {
            addDependency(endBaseline, (index + r.componentIndexDelta, r.start))
          }
      }
    }
    var index = 0
    for (p <- positions) {
      addPosition(index, p)
      index = index + 1
    }
    val (sorted, unsorted) = proofpeer.general.algorithms.TopologicalSort.compute(graph)
    ensure(unsorted.isEmpty, "Cannot resolve dependencies between baselines.") 
    sorted
  }


  def render(parentNode : dom.Node, c : CustomComponent) : Blueprint = {
    val grid = c.attributes(GRID)
    var positions : List[Position] = c.attributes(POSITIONS)
    val dims = c.attributes(DIMS)
    ensure(grid.width >= dims.minimalWidth, "grid is not wide enough")

    var children = c.children.toArray
    ensure(positions.size == children.size, "number of positions and children must match")
    var result : List[Blueprint] = List()

    // compute the start and end baselines for each component
    val componentOrder = computeComponentOrder(positions)
    val numComponents = positions.size
    val startBaselines : Array[Int] = new Array(numComponents)
    val endBaselines : Array[Int] = new Array(numComponents)
    val pos : Array[Position] = positions.toArray
    var numBaselines = 0
    for ((index, start) <- componentOrder) {
      def store(baseline : Int) {
        ensure(baseline >= 0, "baseline must be non-negative")
        if (start) 
          startBaselines(index) = baseline
        else
          endBaselines(index) = baseline   
        if (baseline >= numBaselines) numBaselines = baseline + 1     
      }
      val p = pos(index)
      val baseline = if (start) p.startBaseline else p.endBaseline
      baseline match {
        case Absolute(baseline) => store(baseline)
        case Relative(references, computation) =>
          var baselines : Array[Int] = new Array(references.size)
          var i = 0
          for (r <- references) {
            val baseline = 
              if (r.start) 
                startBaselines(index + r.componentIndexDelta) 
              else
                endBaselines(index + r.componentIndexDelta)
            baselines(i) = baseline 
            i = i + 1
          } 
          store(computation(baselines))
        case Automatic(mode) =>
          val child = children(index)
          val (_, w) = grid.unitExtent(p.startUnit, p.endUnit,
            p.includeLeftGutter, p.includeRightGutter)
          val d = Dimensions(Some(w), None, dims.pixelRatio, None, None, None, None)
          val (_, dy) = RenderTarget.measure(parentNode, child + d.toAttributes)
          var h = dy / grid.baseline
          if (dy % grid.baseline != 0) {
            mode match {
              case CUT => 
              case FILL => h = h + 1
            }
          }
          if (h <= 0) h = 1
          if (start) 
            store(endBaselines(index) - h + 1) 
          else
            store(startBaselines(index) + h - 1)
      }
    }

    // actually render the components
    var index = 0
    while (index < numComponents) {
      val position = pos(index)
      val child = children(index)
      val (x, w) = grid.unitExtent(position.startUnit, position.endUnit,
        position.includeLeftGutter, position.includeRightGutter)
      val attrs : Attributes = 
        if (isAutomatic(position.startBaseline) || isAutomatic(position.endBaseline)) {
          val d = Dimensions(Some(w), None, dims.pixelRatio, None, None, None, None)
          val y1 = grid.baseline * startBaselines(index)
          d.toAttributes + Dimensions.absoluteTopLeft(x, y1)       
        } else { 
          val y1 = grid.baseline * startBaselines(index)
          val y2 = grid.baseline * (1 + endBaselines(index)) - 1
          Dimensions.make(w, y2-y1+1, dims.pixelRatio.get).toAttributes +
            Dimensions.absoluteTopLeft(x, y1)
        }
      result = (child + attrs) :: result
      index = index + 1
    }

    val innerHeight = numBaselines * grid.baseline
    /*var height = innerHeight
    if (height < dims.minimalHeight) height = dims.minimalHeight
    dims.maximalHeight match {
      case None =>
      case Some(h) => if (height > h) height = h
    }

    var width = grid.width
    dims.maximalWidth match {
      case None =>
      case Some(w) =>
        if (w < width) width = w
    }*/

    var resultingChildren = result.reverse

    // optionally visualize the grid
    val showgrid =
      c.attributes.get(SHOW_GRID) match {
        case None => false
        case Some(b) => b
      }
    if (showgrid) {
      val gdims = Dimensions.make(grid.width, innerHeight, dims.pixelRatio)
      val attrs = gdims.toAttributes + Dimensions.absoluteTopLeft(0, 0) + (GRID -> grid)
      resultingChildren = SHOW_GRID_COMPONENT(attrs)() :: resultingChildren
    }
    
    val innerStyle = "overflow:hidden;position:absolute;top:0px;left:0px;width:"+grid.width+"px;height:"+innerHeight+"px"
    val outerStyle = "overflow:hidden;width:"+grid.width+"px;height:"+innerHeight+"px"
    
    if (c.attributes(IS_FORM, false)) {
      FORM(c, STYLE -> outerStyle)(
        DIV(STYLE -> innerStyle)(resultingChildren : _*)
      )
    } else {
      DIV(c, STYLE -> outerStyle)(
        DIV(STYLE -> innerStyle)(resultingChildren : _*)
      )
    }
  }

}

object SHOW_GRID_COMPONENT extends CustomComponentClass {

  val baseline_color = "red"
  val unit_color = "#D5EAF7"
  val gutter_color = "white"

  def rect(color : String, x : Int, y : Int, w : Int, h : Int) : Blueprint = {
    val attrs = Dimensions.make(w, h, 1).toAttributes + Dimensions.absoluteTopLeft(x, y)
    val child = DIV(STYLE->("background-color:"+color))()
    child + attrs
  }

  def render(parentNode : dom.Node, c : CustomComponent) : Blueprint = {
    val grid = c.attributes(GRID_LAYOUT.GRID)
    val dims = c.attributes(DIMS)
    val h = dims.height.get
    var children : List[Blueprint] = List()
    for (u <- 0 until grid.numUnits) {
      children = rect(unit_color, grid.unitX(u), 0, grid.unitWidth(u), h) :: children
      children = rect(gutter_color, grid.leftGutterX(u), 0, grid.leftGutterWidth(u), h) :: children
      children = rect(gutter_color, grid.rightGutterX(u), 0, grid.rightGutterWidth(u), h) :: children
    }
    var b = grid.baseline
    while (b <= h) {
      children = rect(baseline_color, 0, b-1, dims.width.get, 1) :: children
      b = b + grid.baseline
    }
    DIV(c)(children.reverse : _*)
  }

}

