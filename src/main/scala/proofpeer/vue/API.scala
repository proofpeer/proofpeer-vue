package proofpeer.vue

trait Event {
  def eventName : Event.Name
  def info : Any
  def preventDefault()
  def stopPropagation()
  def cancel() {
    preventDefault()
    stopPropagation()
  }
}

object Event {

  class Name(n : String) extends AttributeName[Handler] {
    val name = "event:" + n
    override def toString(value : Any) : String = null
    def read(value : Any) : Option[Handler] = {
      value match {
        case h : Handler => Some(h)
        case _ => None
      }
    }
  }

  trait Handler {
    def handleEvent(component : Component, event : Event)
  }

  type Handlers = Map[Name, Handler]
  
  val NoHandlers : Handlers = Map()
  
  def handle(handlers : (Name, Handler)*) : Handlers = {
    Map(handlers : _*)
  }
  
  case object OnClick extends Name("Click")
  case object OnMouseOver extends Name("MouseOver")
  case object OnMouseEnter extends Name("MouseEnter")
  case object OnMouseLeave extends Name("MouseLeave")
  case object OnSubmit extends Name("Submit")
  case object OnInput extends Name("Input")
  case object OnChange extends Name("Change")

  def publishEvent(origin : Component, eventName : Event.Name, info : Any, performDefault : Event => Unit) {
    Impl.publishEvent(origin, eventName, info, performDefault)
  }

}

sealed case class Blueprint(
  componentClass : ComponentClass,
  eventHandlers : Event.Handlers,
  attributes : Attributes,
  children : Seq[Blueprint]) 
{
  def key : Option[Any] = attributes.get(KEY)
  def +(attrs : Attributes) : Blueprint = {
    Blueprint(componentClass, eventHandlers, attributes + attrs, children)    
  }
  def +(attrs : (AttributeName[Any], Any)*) : Blueprint = {
    this + Attributes(attrs : _*)
  }
  def *(attrs : Attributes) : Blueprint = {
    Blueprint(componentClass, eventHandlers, attributes * attrs, children)    
  }
  def *(attrs : (AttributeName[Any], Any)*) : Blueprint = {
    this * Attributes(attrs : _*)
  }
  def /(attrs : Attributes) : Blueprint = {
    Blueprint(componentClass, eventHandlers, attributes / attrs, children)    
  }
  def /(attrs : (AttributeName[Any], Any)*) : Blueprint = {
    this / Attributes(attrs : _*)
  }
}

sealed abstract class ComponentClass {

  /*******************
   * Basic Interface *
   *******************/

  def name : String

  def createBlueprint(eventHandlers : Event.Handlers, attributes : Attributes, 
    children : Seq[Blueprint]): Blueprint =
  {
    Blueprint(this, eventHandlers, attributes, children)
  }

  final def isPrimitive : Boolean = 
    this match {
      case _ : PrimitiveComponentClass => true
      case _ => false
    }
  
  /***********************************
   *           Apply Syntax          *
   ***********************************/

  final def apply(eventHandlers : Event.Handlers, attributes : Attributes) 
    (children : Blueprint*): Blueprint =
  {
    createBlueprint(eventHandlers, attributes, children)
  }

  final def apply(eventHandlers : Event.Handlers)(children : Blueprint*): Blueprint =
  {
    createBlueprint(eventHandlers, Attributes(), children)
  }

  final def apply(_attributes : Attributes, params : (AttributeName[Any], Any)*) 
    (children : Blueprint*): Blueprint =
  {
    var handlers : Event.Handlers = Event.NoHandlers
    var attributes = _attributes
    for ((name, value) <- params) {
      name match {
        case eventName : Event.Name => 
          handlers = handlers + (eventName -> eventName.read(value).get)
        case _ => 
          val v = name.read(value)
          ensure(v.isDefined, "attribute value for '"+name+"' is not defined")
          attributes = attributes + (name -> v.get)
      }
    }
    createBlueprint(handlers, attributes, children)
  }

  final def apply(params : (AttributeName[Any], Any)*) 
    (children : Blueprint*): Blueprint =
    apply(Attributes(), params : _*)(children : _*)

  final def apply(component : Component, params : (AttributeName[Any], Any)*) 
    (children : Blueprint*): Blueprint =
    apply(component.blueprint.attributes, params : _*)(children : _*)


}

// Components can also be used as keys (for example in maps)
trait Component {
  def blueprint : Blueprint
  def attributes = blueprint.attributes
  def children = blueprint.children
  def getState[T] : T
  def setState(state : Any)
  def mountNode : dom.Node
  def hasUnmounted : Boolean 
  def subComponents : Seq[Component]
  def lookup(key : Any) : Option[Component]
  def apply(key : Any) : Component = lookup(key).get
}

trait CustomComponent extends Component {
  def setLocalState(state : Any)
  def getLocalState() : Any
  def localState[T] : T = getLocalState.asInstanceOf[T]
}

abstract class PrimitiveComponentClass extends ComponentClass {
  // render must depend only on the identity of the component and on component.blueprint
  // the parent node can be used for things like measurements etc.; don't add the rendered node
  // to the parentNode yourself, though! 
  def render(parentNode : dom.Node, component : Component) : dom.Node 
  def updateBlueprint(parentNode : dom.Node, component : Component, newBlueprint : Blueprint, newState : Option[Any])
  def updateState(component : Component, state : Any) {}
  def getState(component : Component) : Any = null
  def didMount(component : Component) {}
  def willUnmount(component : Component) {}
}

abstract class CustomComponentClass extends ComponentClass {
  def name = this.getClass.getName
  // render must depend only on the identity of the component and on component.blueprint and component.getLocalState
  // the parent node can be used for things like measurements etc.; don't add the rendered node
  // to the parentNode yourself, though! 
  def render(parentNode : dom.Node, component : CustomComponent) : Blueprint
  def componentWillMount(component : CustomComponent) {}
  def componentDidMount(component : CustomComponent) {} 
  def componentWillUnmount(component : CustomComponent) {}
  def componentWillUpdate(component : CustomComponent, prevLocalState : Any) {}
  def componentDidUpdate(component : CustomComponent, prevLocalState : Any) {}
  def setState(component : CustomComponent, state : Any) { component.setLocalState(state) }
  def getState(component : CustomComponent) : Any = { component.getLocalState() }
  def componentWillReceiveBlueprint(component : CustomComponent, nextBlueprint : Blueprint) {}
}

trait RenderTarget {
  def render(blueprint : Blueprint) 
  def measure(blueprint : Blueprint) : (Int, Int)
}

object RenderTarget {
  def apply(node : dom.Node) : RenderTarget = Impl.createRenderTarget(node)
  def measure(parentNode : dom.Node, blueprint : Blueprint) : (Int, Int) = {
    this(parentNode).measure(blueprint)
  }
}