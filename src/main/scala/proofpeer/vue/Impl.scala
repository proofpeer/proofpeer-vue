package proofpeer.vue

import scala.collection.immutable.SortedMap

object Impl {

  private var nextVirtualNodeId : Int = 0

  private abstract class VirtualNode(val height : Int) extends Component with Ordered[VirtualNode] {
    val id = nextVirtualNodeId
    nextVirtualNodeId = nextVirtualNodeId + 1
    var blueprint : Blueprint = null
    var mountNode : dom.Node = null
    def willUnmount() {
      EventHandling.updateHandlers(this, blueprint.eventHandlers, Event.NoHandlers)
    }
    def compare(that : VirtualNode) : Int = {
      val h = this.height - that.height
      if (h == 0) this.id - that.id else h
    }
    def executeStateChange(s : Any)
    def lookup(key : Any) : Option[Component] = {
      for (c <- subComponents) {
        if (c.blueprint.key == Some(key)) return Some(c)
      }
      for (c <- subComponents) {
        c.lookup(key) match {
          case None => 
          case found => return found
        }        
      }
      None
    }    
  }

  private class PrimitiveVirtualNode(h : Int) extends VirtualNode(h) {
    var childNodes : Seq[VirtualNode] = null
    def primitiveClass : PrimitiveComponentClass = 
      blueprint.componentClass.asInstanceOf[PrimitiveComponentClass]    
    def setState(s : Any) {
      scheduleStateUpdate(this, s) 
    }
    def getState[T] : T = primitiveClass.getState(this).asInstanceOf[T]
    def executeStateChange(s : Any) { primitiveClass.updateState(this, s) }    
    def subComponents = childNodes
    override def willUnmount() {
      super.willUnmount()
      primitiveClass.willUnmount(this)
      for (child <- childNodes) child.willUnmount() 
    }
  }

  private class CustomVirtualNode(h : Int) extends VirtualNode(h) with CustomComponent {
    var representation : VirtualNode = null
    var _localState : Any = null
    def customClass : CustomComponentClass = 
      blueprint.componentClass.asInstanceOf[CustomComponentClass]
    override def willUnmount() {
      super.willUnmount()
      representation.willUnmount()
      customClass.componentWillUnmount(this)
    }
    def getState[T] : T = customClass.getState(this).asInstanceOf[T]
    def setState(s : Any) { customClass.setState(this, s) }
    def getLocalState() : Any = _localState
    def setLocalState(s : Any) { scheduleStateUpdate(this, s) }
    def executeStateChange(s : Any) { updateState(this, s) }
    def subComponents = Seq(representation)
  }

  private var currentUpdates : SortedMap[VirtualNode, Any] = SortedMap()
  private var currentUpdateTask : Option[Int] = None
  private var runningStateUpdates : Boolean = false

  private def scheduleStateUpdate(virtualNode : VirtualNode, state : Any) {
    currentUpdates = currentUpdates + (virtualNode -> state)    
    if (!runningStateUpdates) {
      currentUpdateTask match {
        case None =>
          currentUpdateTask = Some(dom.setTimeout(() => runStateUpdates(), 0))
        case _ =>
      }
    }
  }

  private def consumeStateUpdate(virtualNode : VirtualNode) : Option[Any] = {
    currentUpdates.get(virtualNode) match {
      case None => None
      case Some(state) =>
        currentUpdates = currentUpdates - virtualNode
        if (currentUpdates.isEmpty) {
          currentUpdateTask match {
            case None =>
            case Some(id) => 
              dom.clearTimeout(id)
              currentUpdateTask = None
          }
        }
        Some(state)
    }
  }

  private def runStateUpdates() {
    // Because new state updates can be inserted into currentUpdates while
    // runStateUpdates is running, this is not guaranteed to terminate.
    // More practical experience is needed to see if that turns out to be a problem.
    // The most problematic scenario seems to be when lower level components change the 
    // state of higher-level components, so that should not happen ever if possible.
    runningStateUpdates = true
    currentUpdateTask = None
    while (!currentUpdates.isEmpty) {
      val (virtualNode, state) = currentUpdates.head
      currentUpdates = currentUpdates - virtualNode
      virtualNode.executeStateChange(state)      
    }
    runningStateUpdates = false
  }

  private def createPrimitiveVirtualNode(height : Int, primitiveClass : PrimitiveComponentClass, 
    blueprint : Blueprint) : PrimitiveVirtualNode = 
  {
    val vnode = new PrimitiveVirtualNode(height)
    vnode.blueprint = blueprint
    vnode.mountNode = primitiveClass.render(vnode)
    val children = blueprint.children.map(c => createVirtualNode(height + 1, c))
    for (child <- children) {
      vnode.mountNode.appendChild(child.mountNode)
    }
    vnode.childNodes = children
    EventHandling.updateHandlers(vnode, Event.NoHandlers, blueprint.eventHandlers)   
    primitiveClass.didMount(vnode) 
    return vnode
  }

  private def createCustomVirtualNode(height : Int, customClass : CustomComponentClass, 
    blueprint : Blueprint) : CustomVirtualNode = 
  {
    val vnode = new CustomVirtualNode(height)
    vnode.blueprint = blueprint
    customClass.componentWillMount(vnode)
    val optState = consumeStateUpdate(vnode)
    if (optState.isDefined) vnode._localState = optState.get
    val representingBlueprint = customClass.render(vnode)
    vnode.representation = createVirtualNode(height + 1, representingBlueprint)
    vnode.mountNode = vnode.representation.mountNode
    EventHandling.updateHandlers(vnode, Event.NoHandlers, blueprint.eventHandlers)
    customClass.componentDidMount(vnode)
    return vnode
  }

  private def createVirtualNode(height : Int, blueprint : Blueprint) : VirtualNode = {
    blueprint.componentClass match {
      case primitiveClass : PrimitiveComponentClass =>
        createPrimitiveVirtualNode(height, primitiveClass, blueprint)
      case customClass : CustomComponentClass =>
        createCustomVirtualNode(height, customClass, blueprint)
    }
  }

  private def differentComponentClasses(u : ComponentClass, v : ComponentClass) : Boolean = {
    (u.isPrimitive != v.isPrimitive) || (u.name.toLowerCase != v.name.toLowerCase)
  }

  private def updateState(customNode : CustomVirtualNode, state : Any) {
    if (customNode._localState == state) return
    val oldState = customNode._localState
    val cl = customNode.customClass
    cl.componentWillUpdate(customNode, state)
    customNode._localState = state
    val newRepBlueprint = cl.render(customNode)
    val activeElement = customNode.mountNode.activeElement
    customNode.representation = updateBlueprint(activeElement, customNode.representation, newRepBlueprint)
    customNode.mountNode = customNode.representation.mountNode
    cl.componentDidUpdate(customNode, oldState)
  }

  private class OrderedVirtualNode(
    val vnode : VirtualNode, 
    val oldPosition : Option[(Int, Boolean)],
    var marked : Boolean = false) extends Ordered[OrderedVirtualNode]
  {
    def compare(that : OrderedVirtualNode) : Int = {
      this.oldPosition.get._1 - that.oldPosition.get._1  
    }
  }

  private def updateBlueprints(activeElement : Option[dom.Node], parentHeight : Int,
    parentNode : dom.Node, blueprints : Seq[Blueprint], nodes : Seq[VirtualNode]) : Seq[VirtualNode] =
  {
    type Key = Any
    var keyedNodes : Map[Key, (VirtualNode, Int)] = Map()
    var leftNodes : List[(VirtualNode, Int)] = List()
    var i : Int = 0
    for (node <- nodes) {
      node.blueprint.key match {
        case None => leftNodes = (node, i) :: leftNodes
        case Some(key) =>
          keyedNodes.get(key) match {
            case None =>
              keyedNodes = keyedNodes + (key -> (node, i)) 
            case _ =>
              throw new RuntimeException("duplicate key among node children: "+key)
          }
      }
      i = i + 1
    }
    leftNodes = leftNodes.reverse
    var result : List[OrderedVirtualNode] = List()
    var existing : List[OrderedVirtualNode] = List()
    var count : Int = 0
    var activeElementPosition : Option[Int] = None
    def addExisting(virtualNode : VirtualNode, i : Int, blueprint : Blueprint) {
      val ae : Option[dom.Node] = 
        activeElement match {
          case None => None
          case Some(ae) =>
            if (virtualNode.mountNode.contains(ae))
              activeElement
            else
              None
        }
      val vnode = updateBlueprint(ae, virtualNode, blueprint)
      val oldPosition = Some(i, ae.isDefined)
      val orderedVirtualNode = new OrderedVirtualNode(vnode, oldPosition)
      result = orderedVirtualNode :: result
      existing = orderedVirtualNode :: existing
      if (ae.isDefined) activeElementPosition = Some(count)
      count = count + 1
    }
    def addNew(blueprint : Blueprint) {
      val virtualNode = createVirtualNode(parentHeight + 1, blueprint)
      result = new OrderedVirtualNode(virtualNode, None) :: result
    }    
    for (blueprint <- blueprints) {
      blueprint.key match {
        case None => 
          leftNodes match {
            case (virtualNode, i) :: others => 
              addExisting(virtualNode, i, blueprint)
              leftNodes = others
            case _ => addNew(blueprint)
          }
        case Some(key) =>
          keyedNodes.get(key) match {
            case None => addNew(blueprint)
            case Some((virtualNode, i)) =>
              addExisting(virtualNode, i, blueprint)
              keyedNodes = keyedNodes - key
          }
      }
    }
    def remove(virtualNode : VirtualNode) {
      virtualNode.willUnmount()
      parentNode.removeChild(virtualNode.mountNode)
    }
    for ((virtualNode, _) <- leftNodes) remove(virtualNode)
    for ((_, (virtualNode, _)) <- keyedNodes) remove(virtualNode)
    import proofpeer.general.algorithms.LongestNondecreasingSubsequence.compute
    val increasingNodes = 
      activeElementPosition match {
        case None => compute(existing.reverse)
        case Some(position) => compute(existing.reverse, count - position - 1)
      }
    for (node <- increasingNodes) node.marked = true
    var lastNode : dom.Node = null
    for (node <- result) {
      if (!node.marked) {
        if (lastNode == null) 
          parentNode.appendChild(node.vnode.mountNode)
        else 
          parentNode.insertBefore(node.vnode.mountNode, lastNode)
      }
      lastNode = node.vnode.mountNode
    }
    return result.reverse.map(_.vnode)
  }

  private def updateBlueprint(activeElement : Option[dom.Node], 
    virtualNode : VirtualNode, blueprint : Blueprint) : VirtualNode = 
  {
    if (virtualNode.blueprint == blueprint) return virtualNode
    if (differentComponentClasses(virtualNode.blueprint.componentClass, blueprint.componentClass)) {
      val newVirtualNode = createVirtualNode(virtualNode.height, blueprint)
      virtualNode.willUnmount()
      newVirtualNode.mountNode.replace(virtualNode.mountNode)
      return newVirtualNode
    } else {
      virtualNode match {
        case primitiveNode : PrimitiveVirtualNode =>
          val optState = consumeStateUpdate(primitiveNode)
          primitiveNode.primitiveClass.updateBlueprint(primitiveNode, blueprint, optState)
          primitiveNode.childNodes = updateBlueprints(activeElement, virtualNode.height,
            primitiveNode.mountNode, blueprint.children, primitiveNode.childNodes)
        case customNode : CustomVirtualNode =>
          val cl = customNode.customClass
          var optState = consumeStateUpdate(customNode)
          cl.componentWillReceiveBlueprint(customNode, blueprint, optState)
          consumeStateUpdate(customNode) match {
            case None =>
            case s => optState = s
          }
          if (optState.isDefined) customNode._localState = optState.get
          customNode.blueprint = blueprint
          val newRepBlueprint = customNode.customClass.render(customNode)
          customNode.representation = updateBlueprint(activeElement, customNode.representation, newRepBlueprint)
          customNode.mountNode = customNode.representation.mountNode
      }
      EventHandling.updateHandlers(virtualNode, virtualNode.blueprint.eventHandlers, blueprint.eventHandlers)
      virtualNode.blueprint = blueprint
      return virtualNode
    }
  }

  def publishEvent(origin : Component, eventName : Event.Name, info : Any, performDefault : Event => Unit) {
    val target = origin.mountNode.inner
    val event = new EventHandling.SyntheticEvent(eventName, info, null)
    dom.setTimeout(() => EventHandling.processEvent(event, Some(performDefault), target), 0)  
  }  

  private object EventHandling {

    import scala.scalajs.js
    
    var eventHandlers : Map[Event.Name, Map[VirtualNode, Event.Handler]] = Map()

    def updateHandlers(virtualNode : VirtualNode, 
      oldHandlers : Event.Handlers, newHandlers : Event.Handlers) 
    {
      val oldEventNames = oldHandlers.keySet
      val newEventNames = newHandlers.keySet
      for (eventName <- oldEventNames -- newEventNames) {
        val m = eventHandlers(eventName) - virtualNode
        if (m.isEmpty) {
          eventHandlers = eventHandlers - eventName
          stopListeningToEvent(eventName)
        } else {
          eventHandlers = eventHandlers + (eventName -> m)
        }
      }
      for (eventName <- newEventNames -- oldEventNames) {
        eventHandlers.get(eventName) match {
          case None =>
            val m = Map(virtualNode -> newHandlers(eventName))
            eventHandlers = eventHandlers + (eventName -> m)
            startListeningToEvent(eventName)
          case Some(m) =>
            val updatedM = m + (virtualNode -> newHandlers(eventName))
            eventHandlers = eventHandlers + (eventName -> updatedM)
        }
      }
    }

    def nativeName(eventName : Event.Name) : Option[String] = {
      Some(
        eventName match {
          case Event.OnClick => "click"
          case Event.OnSubmit => "submit"          
          case _ => return None
        })
    }

    def syntheticEvent(nativeEvent : js.Dynamic) : Option[SyntheticEvent] = {
      val nativeName : String = nativeEvent.`type`.asInstanceOf[String]
      Some(
        nativeName.toLowerCase match {
          case "click" => 
            new SyntheticEvent(Event.OnClick, null, nativeEvent)
          case "submit" => 
            new SyntheticEvent(Event.OnSubmit, null, nativeEvent)
          case _ => return None  
        })
    }

    class SyntheticEvent(val eventName : Event.Name, val info : Any, nativeEvent : js.Dynamic) extends Event 
    {
      var cancelled : Boolean = nativeEvent.defaultPrevented.asInstanceOf[Boolean]

      var bubbling : Boolean = true

      def preventDefault() {
        if (!cancelled && nativeEvent.cancelable.asInstanceOf[Boolean]) {
          cancelled = true
          nativeEvent.preventDefault()
        }
      }

      def stopPropagation() {
        if (bubbling) {
          bubbling = false
          nativeEvent.stopPropagation()
        }
      }

    }

    object handlerOrdering extends Ordering[(VirtualNode, Event.Handler)] {
      def compare(u : (VirtualNode, Event.Handler), v : (VirtualNode, Event.Handler)) : Int = {
        v._1.height - u._1.height
      }
    }

    def processEvent(event : SyntheticEvent, defaultAction : Option[Event => Unit], target : js.Dynamic) {
      val m = eventHandlers(event.eventName)
      var hits : List[(VirtualNode, Event.Handler)] = List()
      for ((virtualNode, handler) <- m) {
        if (virtualNode.mountNode.inner.contains(target).asInstanceOf[Boolean]) {
          hits = (virtualNode, handler) :: hits
        }
      }
      val hitsArray = hits.toArray
      scala.util.Sorting.quickSort(hitsArray)(handlerOrdering)
      def handleEvent() {
        for ((virtualNode, handler) <- hitsArray) {
          handler.handleEvent(virtualNode, event)
          if (!event.bubbling) {
            if (!event.cancelled && defaultAction.isDefined) {
              (defaultAction.get)(event)
            }
            return
          }
        }
        if (!event.cancelled && defaultAction.isDefined) {
          (defaultAction.get)(event)
        }        
      }
      handleEvent()      
    }

    val listener : js.Function1[js.Dynamic, Unit] = (nativeEvent : js.Dynamic) => {
      syntheticEvent(nativeEvent) match {
        case None => 
          println("cannot handle event type: "+nativeEvent.`type`)
        case Some(event) =>
          processEvent(event, None, nativeEvent.target)
      }
    }

    private def startListeningToEvent(eventName : Event.Name) {
      nativeName(eventName) match {
        case None =>
          println("warning: no native event found for: "+eventName)
        case Some(nativeName) =>
          js.Dynamic.global.document.addEventListener(nativeName, listener, true)
      }
    }

    private def stopListeningToEvent(eventName : Event.Name) {
      nativeName(eventName) match {
        case None =>
          println("warning: no native event found for: "+eventName)
        case Some(nativeName) =>
          js.Dynamic.global.document.removeEventListener(nativeName, listener)
      }
    }

  }

  private class Target(domNode : dom.Node) extends RenderTarget {

    var virtualNode : VirtualNode = null

    def render(blueprint : Blueprint) {
      if (virtualNode == null) {
        virtualNode = createVirtualNode(0, blueprint)
        domNode.appendChild(virtualNode.mountNode)
      } else 
        virtualNode = updateBlueprint(virtualNode.mountNode.activeElement, virtualNode, blueprint)
    }

  }

  def createRenderTarget(node : dom.Node) : RenderTarget = {
    return new Target(node)
  }

}