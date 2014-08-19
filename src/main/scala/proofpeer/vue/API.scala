package proofpeer.vue

trait Event {
  def eventName : Event.Name
  def info : Any
  def preventDefault()
  def stopPropagation()
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
  case object OnSubmit extends Name("Submit")

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
  def attribute[T] : T = attributes[T]()
  def key : Option[Any] = attributes.get(AttributeName.KEY)
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
   * Apply Syntax *
   ***********************************/

  final def apply(eventHandlers : Event.Handlers, attributes : Attributes) 
    (children : Blueprint*): Blueprint =
  {
    createBlueprint(eventHandlers, attributes, children)
  }

  final def apply(attributes : Attributes)(children : Blueprint*): Blueprint =
  {
    createBlueprint(Event.NoHandlers, attributes, children)
  }

  final def apply(eventHandlers : Event.Handlers)(children : Blueprint*): Blueprint =
  {
    createBlueprint(eventHandlers, Attributes(), children)
  }

  final def apply(params : (AttributeName[Any], Any)*) 
    (children : Blueprint*): Blueprint =
  {
    var handlers : Event.Handlers = Map()
    var attributes : Attributes = Attributes()
    for ((name, value) <- params) {
      name match {
        case eventName : Event.Name => 
          handlers = handlers + (eventName -> eventName.read(value).get)
        case _ => 
          attributes = attributes + (name -> name.read(value).get)
      }
    }
    createBlueprint(handlers, attributes, children)
  }


}

// Components can also be used as keys (for example in maps)
trait Component {
  def blueprint : Blueprint
  def getState[T] : T
  def setState(state : Any)
  def mountNode : dom.Node
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
  def render(component : Component) : dom.Node
  def updateBlueprint(component : Component, newBlueprint : Blueprint, newState : Option[Any])
  def updateState(component : Component, state : Any) {}
  def getState(component : Component) : Any = null
  def didMount(component : Component) {}
  def willUnmount(component : Component) {}
}

abstract class CustomComponentClass extends ComponentClass {
  def name = this.getClass.getName
  // render must depend only on the identity of the component and on component.blueprint and component.getLocalState
  def render(component : CustomComponent) : Blueprint
  def componentWillMount(component : CustomComponent) {}
  def componentDidMount(component : CustomComponent) {} 
  def componentWillUnmount(component : CustomComponent) {}
  def componentWillUpdate(component : CustomComponent, nextLocalState : Any) {}
  def componentDidUpdate(component : CustomComponent, prevLocalState : Any) {}
  def setState(component : CustomComponent, state : Any) { component.setLocalState(state) }
  def getState(component : CustomComponent) : Any = { component.getLocalState() }
  def componentWillReceiveBlueprint(component : CustomComponent, nextBlueprint : Blueprint, newLocalState : Option[Any]) {}
}

trait RenderTarget {
  def render(blueprint : Blueprint) 
}

object RenderTarget {
  def apply(node : dom.Node) : RenderTarget = Impl.createRenderTarget(node)
}



