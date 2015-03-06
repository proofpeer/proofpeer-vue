package proofpeer.vue

import scala.collection.immutable.SortedMap
import dom.Node

object Impl {  

  private var nextVirtualNodeId : Int = 0

  private abstract class VirtualNode(val height : Int) extends Component with Ordered[VirtualNode] {
    val id = nextVirtualNodeId
    nextVirtualNodeId = nextVirtualNodeId + 1
    var blueprint : Blueprint = null
    var mountNode : dom.Node = null
    private var hasStartedUnmounting : Boolean = false
    def hasUnmounted : Boolean = hasStartedUnmounting
    def willUnmount() {
      hasStartedUnmounting = true
      EventHandling.updateHandlers(this, blueprint.eventHandlers, Event.NoHandlers)
    }
    def compare(that : VirtualNode) : Int = {
      val h = this.height - that.height
      if (h == 0) this.id - that.id else h
    }
    override def equals(that : Any) : Boolean = {
      that match {
        case v : VirtualNode => compare(v) == 0
        case _ => false
      }
    }
    override def hashCode : Int = { List(height, id).hashCode() }
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
    var _prevLocalState : Option[Any] = None
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
    def setLocalState(s : Any) { 
      if (!_prevLocalState.isDefined)
        _prevLocalState = Some(_localState)
      _localState = s
      scheduleStateUpdate(this, s) 
    }
    def executeStateChange(s : Any) { 
      updateState(this, s) 
      _prevLocalState = None
    }
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

  private def createPrimitiveVirtualNode(parentNode : dom.Node, height : Int, primitiveClass : PrimitiveComponentClass, 
    blueprint : Blueprint) : PrimitiveVirtualNode = 
  {
    val vnode = new PrimitiveVirtualNode(height)
    vnode.blueprint = blueprint
    vnode.mountNode = primitiveClass.render(parentNode, vnode)
    parentNode.appendChild(vnode.mountNode)
    val children = blueprint.children.map(c => createVirtualNode(vnode.mountNode, height + 1, c))
    vnode.childNodes = children
    EventHandling.updateHandlers(vnode, Event.NoHandlers, blueprint.eventHandlers)   
    primitiveClass.didMount(vnode) 
    return vnode
  }

  private def createCustomVirtualNode(parentNode : dom.Node, height : Int, customClass : CustomComponentClass, 
    blueprint : Blueprint) : CustomVirtualNode = 
  {
    val vnode = new CustomVirtualNode(height)
    vnode.blueprint = blueprint
    customClass.componentWillMount(vnode)
    consumeStateUpdate(vnode)
    val representingBlueprint = customClass.render(parentNode, vnode)
    vnode.representation = createVirtualNode(parentNode, height + 1, representingBlueprint)
    vnode.mountNode = vnode.representation.mountNode
    EventHandling.updateHandlers(vnode, Event.NoHandlers, blueprint.eventHandlers)
    customClass.componentDidMount(vnode)
    return vnode
  }

  private def createVirtualNode(parentNode : dom.Node, height : Int, blueprint : Blueprint) : VirtualNode = {
    blueprint.componentClass match {
      case primitiveClass : PrimitiveComponentClass =>
        createPrimitiveVirtualNode(parentNode, height, primitiveClass, blueprint)
      case customClass : CustomComponentClass =>
        createCustomVirtualNode(parentNode, height, customClass, blueprint)
    }
  }

  private def differentComponentClasses(u : ComponentClass, v : ComponentClass) : Boolean = {
    (u.isPrimitive != v.isPrimitive) || (u.name.toLowerCase != v.name.toLowerCase)
  }

  private def updateState(customNode : CustomVirtualNode, state : Any) {
    if (customNode._prevLocalState == Some(state)) return
    val oldstate = customNode._prevLocalState.get
    val cl = customNode.customClass
    cl.componentWillUpdate(customNode, oldstate)
    customNode._localState = state
    val newRepBlueprint = cl.render(customNode.mountNode.parentNode, customNode)
    val activeElement = customNode.mountNode.activeElement
    customNode.representation = updateBlueprint(activeElement, customNode.representation, newRepBlueprint)
    customNode.mountNode = customNode.representation.mountNode
    cl.componentDidUpdate(customNode, oldstate)
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
      val virtualNode = createVirtualNode(parentNode, parentHeight + 1, blueprint)
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
      val newVirtualNode = createVirtualNode(virtualNode.mountNode.parentNode, virtualNode.height, blueprint)
      virtualNode.willUnmount()
      newVirtualNode.mountNode.replace(virtualNode.mountNode)
      return newVirtualNode
    } else {
      virtualNode match {
        case primitiveNode : PrimitiveVirtualNode =>
          val optState = consumeStateUpdate(primitiveNode)
          primitiveNode.primitiveClass.updateBlueprint(primitiveNode.mountNode.parentNode, primitiveNode, blueprint, optState)
          primitiveNode.childNodes = updateBlueprints(activeElement, virtualNode.height,
            primitiveNode.mountNode, blueprint.children, primitiveNode.childNodes)
        case customNode : CustomVirtualNode =>
          val cl = customNode.customClass
          consumeStateUpdate(customNode)
          cl.componentWillReceiveBlueprint(customNode, blueprint)
          customNode.blueprint = blueprint
          val newRepBlueprint = customNode.customClass.render(customNode.mountNode.parentNode, customNode)
          customNode.representation = updateBlueprint(activeElement, customNode.representation, newRepBlueprint)
          customNode.mountNode = customNode.representation.mountNode
      }
      EventHandling.updateHandlers(virtualNode, virtualNode.blueprint.eventHandlers, blueprint.eventHandlers)
      virtualNode.blueprint = blueprint
      return virtualNode
    }
  }

  def publishEvent(origin : Component, eventName : Event.Name, info : Any, performDefault : Event => Unit) {
    val event = new EventHandling.SyntheticEvent(eventName, info, null)
    dom.setTimeout(() => EventHandling.processEvent(event, Some(performDefault), origin.mountNode), 0)  
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
      for (eventName <- newEventNames) {
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
          case Event.OnInput => "input" 
          case Event.OnChange => "change"
          case Event.OnMouseOver => "mouseover"
          case Event.OnMouseEnter => "mouseenter"
          case Event.OnMouseLeave => "mouseleave"   
          case Event.OnKeyPress => "keypress"
          case Event.OnKeyDown => "keydown"
          case Event.OnKeyUp => "keyup"  
          case Event.OnDragOver => "dragover"
          case Event.OnDrop => "drop"             
          case _ => return None
        })
    }

    final class KeyInfo(val key : String, val keyCode : Int, val chars : String) extends Event.KeyInfo {
      override def toString : String = "KeyInfo(key="+key+",keyCode="+keyCode+",chars='"+chars+"')"
    }

    /** Currently just Windows, see: https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key#Key_values */
    def keyValue(code : Int) : String = {
      code match {
        case 0x03 => "Cancel"
        case 0x08 => "Backspace"
        case 0x09 => "Tab"
        case 0x0C => "Clear"
        case 0x0D => "Enter"
        case 0x10 => "Shift"
        case 0x11 => "Control"
        case 0x12 => "Alt"
        case 0x13 => "Pause"
        case 0x14 => "CapsLock"
        case 0x1B => "Escape"
        case 0x1C => "Convert"
        case 0x1D => "NonConvert"
        case 0x1E => "Accept"
        case 0x1F => "ModeChange"
        case 0x20 => " "
        case 0x21 => "PageUp"
        case 0x22 => "PageDown"
        case 0x23 => "End"
        case 0x24 => "Home"
        case 0x25 => "ArrowLeft"
        case 0x26 => "ArrowUp"
        case 0x27 => "ArrowRight"
        case 0x28 => "ArrowDown"
        case 0x29 => "Select"
        case 0x2B => "Execute"
        case 0x2C => "PrintScreen"
        case 0x2D => "Insert"
        case 0x2E => "Delete"
        case 0x2F => "Help"
        case 0x5B => "OS"
        case 0x5C => "OS"
        case 0x5D => "ContextMenu"
        case 0x5F => "Standby"
        case 0x60 => "0"
        case 0x61 => "1"
        case 0x62 => "2"
        case 0x63 => "3"
        case 0x64 => "4"
        case 0x65 => "5"
        case 0x66 => "6"
        case 0x67 => "7"
        case 0x68 => "8"
        case 0x69 => "9"
        case 0x6A => "*"
        case 0x6B => "+"
        case 0x6C => "Separator"
        case 0x6D => "-"
        case 0x6E => "."
        case 0x6F => "/"
        case 0x70 => "F1"
        case 0x71 => "F2"
        case 0x72 => "F3"
        case 0x73 => "F4"
        case 0x74 => "F5"
        case 0x75 => "F6"
        case 0x76 => "F7"
        case 0x77 => "F8"
        case 0x78 => "F9"
        case 0x79 => "F10"
        case 0x7A => "F11"
        case 0x7B => "F12"
        case 0x7C => "F13"
        case 0x7D => "F14"
        case 0x7E => "F15"
        case 0x7F => "F16"
        case 0x80 => "F17"
        case 0x81 => "F18"
        case 0x82 => "F19"
        case 0x83 => "F20"
        case 0x84 => "F21"
        case 0x85 => "F22"
        case 0x86 => "F23"
        case 0x87 => "F24"
        case 0x90 => "NumLock"
        case 0x91 => "ScrollLock"
        case 0xA0 => "Shift"
        case 0xA1 => "Shift"
        case 0xA2 => "Control"
        case 0xA3 => "Control"
        case 0xA4 => "Alt"
        case 0xA5 => "Alt"
        case 0xA6 => "BrowserBack"
        case 0xA7 => "BrowserForward"
        case 0xA8 => "BrowserRefresh"
        case 0xA9 => "BrowserStop"
        case 0xAA => "BrowserSearch"
        case 0xAB => "BrowserFavorites"
        case 0xAC => "BrowserHome"
        case 0xAD => "VolumeMute"
        case 0xAE => "VolumeDown"
        case 0xAF => "VolumeUp"
        case 0xB0 => "MediaTrackNext"
        case 0xB1 => "MediaTrackPrevious"
        case 0xB2 => "MediaStop"
        case 0xB3 => "MediaPlayPause"
        case 0xB4 => "LaunchMail"
        case 0xB5 => "MediaSelect"
        case 0xB6 => "LaunchApplication1"
        case 0xB7 => "LaunchApplication2"
        case 0xF7 => "CrSel"
        case 0xF8 => "ExSel"
        case 0xF9 => "EraseOf"
        case 0xFA => "Play"
        case 0xFB => "ZoomToggle"
        case 0xFE => "Clear"
        case _ => "Unidentified"
      }
    }

    def keyChangeInfo(event : js.Dynamic) : Event.KeyInfo = {
      val keyCode : Int = event.keyCode.asInstanceOf[Int]
      val key : String = keyValue(keyCode)
      new KeyInfo(key, keyCode, "")
    }

    def keyPressInfo(event : js.Dynamic) : Event.KeyInfo = {
      val k = event.charCode.asInstanceOf[Int]
      new KeyInfo("", k, k.toChar.toString)
    }


    def syntheticEvent(nativeEvent : js.Dynamic) : Option[SyntheticEvent] = {
      val nativeName : String = nativeEvent.`type`.asInstanceOf[String]
      Some(
        nativeName.toLowerCase match {
          case "click" => 
            new SyntheticEvent(Event.OnClick, null, nativeEvent)
          case "submit" => 
            new SyntheticEvent(Event.OnSubmit, null, nativeEvent)
          case "input" =>
            new SyntheticEvent(Event.OnInput, null, nativeEvent)
          case "change" =>
            new SyntheticEvent(Event.OnChange, null, nativeEvent)
          case "mouseover" =>
            new SyntheticEvent(Event.OnMouseOver, null, nativeEvent, true)
          case "mouseenter" =>
            new SyntheticEvent(Event.OnMouseEnter, null, nativeEvent, true)
          case "mouseleave" =>
            new SyntheticEvent(Event.OnMouseLeave, null, nativeEvent, true)
          case "keydown" =>
            new SyntheticEvent(Event.OnKeyDown, keyChangeInfo(nativeEvent), nativeEvent)
          case "keypress" =>
            new SyntheticEvent(Event.OnKeyPress, keyPressInfo(nativeEvent), nativeEvent)
          case "keyup" =>
            new SyntheticEvent(Event.OnKeyUp, keyChangeInfo(nativeEvent), nativeEvent)
          case "drop" => 
            new SyntheticEvent(Event.OnDrop, null, nativeEvent)
          case "dragover" => 
            new SyntheticEvent(Event.OnDragOver, null, nativeEvent)
          case _ => return None
        })
    }

    class SyntheticEvent(val eventName : Event.Name, val info : Any, val nativeEvent : js.Dynamic, val isExact : Boolean = false) extends Event 
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

    def processEvent(event : SyntheticEvent, defaultAction : Option[Event => Unit], target : Node) {
      val m : Map[VirtualNode, Event.Handler] = eventHandlers(event.eventName)
      var hits : List[(VirtualNode, Event.Handler)] = List()
      for ((virtualNode, handler) <- m) {
        if (virtualNode.mountNode.contains(target) && (!event.isExact || target.contains(virtualNode.mountNode))) {
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
          processEvent(event, None, Node.make(nativeEvent.target))
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
          js.Dynamic.global.document.removeEventListener(nativeName, listener, true)
      }
    }

  }

  private class Target(domNode : dom.Node) extends RenderTarget {

    var virtualNode : VirtualNode = null

    def render(blueprint : Blueprint) {
      if (domNode != null && domNode.isInDom) {
        if (virtualNode == null) 
          virtualNode = createVirtualNode(domNode, 0, blueprint)
        else 
          virtualNode = updateBlueprint(virtualNode.mountNode.activeElement, virtualNode, blueprint)
      } else {
        println("trying to render into invalid dom node")
      }
    }

    def measure(_blueprint : Blueprint) : (Int, Int) = {
      if (domNode != null && domNode.isInDom) {
        val blueprint = _blueprint + (dom.STYLE -> "visibility:hidden;position:absolute;display:block")
        val vn = createVirtualNode(domNode, 0, blueprint)
        val w = vn.mountNode().offsetWidth.asInstanceOf[Int]
        val h = vn.mountNode().offsetHeight.asInstanceOf[Int]
        domNode.removeChild(vn.mountNode)
        (w, h)
      } else {
        println("trying to measure in invalid dom node")
        (0,0)
      }
    }

  }

  def createRenderTarget(node : dom.Node) : RenderTarget = {
    return new Target(node)
  }

}