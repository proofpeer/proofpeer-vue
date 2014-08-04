package proofpeer.vue

trait Parameters
case object NoParameters extends Parameters

trait Event {
  def eventName : Event.Name
  def info : Any
  def preventDefault()
  def stopPropagation()
}

object Event {
  trait Name

  trait Handler {
    def handleEvent(component : Component, event : Event)
  }

  type Handlers = Map[Name, Handler]
  
  val NoHandlers : Handlers = Map()
  
  def handle(handlers : (Name, Handler)*) : Handlers = {
    Map(handlers : _*)
  }
  
  case object onClick extends Name
  case object onSubmit extends Name

  def publishEvent(origin : Component, eventName : Event.Name, info : Any, performDefault : Event => Unit) {
    Impl.publishEvent(origin, eventName, info, performDefault)
  }

}

trait Key
case object NoKey extends Key
case class StringKey(s : String) extends Key
case class IntKey(i : Int) extends Key

sealed case class Blueprint(
  componentClass : ComponentClass,
  key : Key,
  parameters : Parameters,
  eventHandlers : Event.Handlers,
  children : Seq[Blueprint]) 
{
  def apply[T]() = parameters.asInstanceOf[T]
  def hasParams : Boolean = parameters != NoParameters
}

sealed abstract class ComponentClass {

  /*******************
   * Basic Interface *
   *******************/

  def name : String

  def createBlueprint(key : Key, parameters : Parameters, 
    eventHandlers : Event.Handlers, children : Seq[Blueprint]): Blueprint =
  {
    Blueprint(this, key, parameters, eventHandlers, children)
  }

  final def isPrimitive : Boolean = 
    this match {
      case _ : PrimitiveComponentClass => true
      case _ => false
    }
  
  /***********************************
   * Implementations of apply Syntax *
   ***********************************/

  final def apply(key : Key, parameters : Parameters, 
    eventHandlers : Event.Handlers) (children : Blueprint*): Blueprint =
  {
    createBlueprint(key, parameters, eventHandlers, children)
  }
  
  final def apply(key : Key, parameters : Parameters)(children : Blueprint*): Blueprint =
  {
    createBlueprint(key, parameters, Event.NoHandlers, children)
  }

  final def apply(key : Key, eventHandlers : Event.Handlers)(children : Blueprint*): Blueprint =
  {
    createBlueprint(key, NoParameters, eventHandlers, children)
  }

  final def apply(key : Key)(children : Blueprint*): Blueprint =
  {
    createBlueprint(key, NoParameters, Event.NoHandlers, children)
  }

  final def apply(parameters : Parameters, eventHandlers : Event.Handlers)
    (children : Blueprint*): Blueprint =
  {
    createBlueprint(NoKey, parameters, eventHandlers, children)
  }
  
  final def apply(parameters : Parameters)(children : Blueprint*): Blueprint =
  {
    createBlueprint(NoKey, parameters, Event.NoHandlers, children)
  }

  final def apply(eventHandlers : Event.Handlers)(children : Blueprint*): Blueprint =
  {
    createBlueprint(NoKey, NoParameters, eventHandlers, children)
  }

  final def apply()(children : Blueprint*): Blueprint =
  {
    createBlueprint(NoKey, NoParameters, Event.NoHandlers, children)
  }

}

// Components can also be used as keys (for example in maps)
trait Component {
  def blueprint : Blueprint
  def getState[T] : T
  def setState(state : Any)
  def mountNode : DOM.Node
  def subComponents : Seq[Component]
  def lookup(key : Key) : Option[Component]
  def apply(key : Key) : Component = lookup(key).get
  def apply(key : String) : Component = apply(StringKey(key))
  def apply(key : Int) : Component = apply(IntKey(key))
}

trait CustomComponent extends Component {
  def setLocalState(state : Any)
  def getLocalState() : Any
}

abstract class PrimitiveComponentClass extends ComponentClass {
  // render must depend only on the identity of the component and on component.blueprint
  def render(component : Component) : DOM.Node
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
  def apply(node : DOM.Node) : RenderTarget = Impl.createRenderTarget(node)
}



