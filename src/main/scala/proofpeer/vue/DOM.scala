package proofpeer.vue

object DOM {

  import scala.scalajs.js

  class Node(private val node : js.Dynamic) {

    def appendChild(child : Node) {
      node.appendChild(child.node)
    }

    def countChildren : Int = node.childNodes.length.asInstanceOf[Int]

    // this assumes that child is not a child of the node yet
    def insertNewChild(child : Node, position : Int) {
      val count = countChildren
      if (position == count) appendChild(child)
      else if (position >= 0 && position < count) {
        node.insertBefore(child.node, node.childNodes.item(position))
      } else throw new RuntimeException("invalid position for DOM.Node.insert: " + position)
    }

    def insertBefore(child : Node, before : Node) {
      node.insertBefore(child.node, before.node)
    }

    def replace(nodeToReplace : Node) {
      val parent = nodeToReplace.node.parentNode
      parent.replaceChild(node, nodeToReplace.node)
    }

    def removeChild(child : Node) {
      node.removeChild(child.node)
    }

    def contains(that : Node) : Boolean = {
      node.contains(that.node).asInstanceOf[Boolean]
    }

    // returns the active element if there is one contained in this node
    def activeElement : Option[Node] = {
      val a = document.activeElement
      if (a == null) None
      else if (node.contains(a).asInstanceOf[Boolean]) Some(mkNode(a))
      else None
    }

    def inner : js.Dynamic = node
  }

  private def mkNode(node : js.Dynamic) : Node = {
    new Node(node)
  }

  private def document : js.Dynamic = js.Dynamic.global.document

  private def window : js.Dynamic = js.Dynamic.global.window

  def lookupNode(id : String) : Option[Node] = { 
    val node = document.getElementById(id)
    if (node == null) None else Some(new Node(node))
  }

  def setInterval(f : () => Unit, millis : Int) : Int = {
    window.setInterval(f, millis).asInstanceOf[Int]
  }

  def clearInterval(intervalId : Int) {
    window.clearInterval(intervalId)
  }

  def setTimeout(f : () => Unit, millis : Int) : Int = {
    window.setTimeout(f, millis).asInstanceOf[Int]
  }

  def clearTimeout(intervalId : Int) {
    window.clearTimeout(intervalId)
  }  

  class DefaultPrimitiveComponent(val name : String) 
    extends PrimitiveComponentClass
  {

    private def attributesOf(blueprint : Blueprint) : Attributes = {
      if (blueprint.hasParams) blueprint() else Attributes()
    }

    def render(component : Component) : DOM.Node = {
      val attributes = attributesOf(component.blueprint)
      val elem = document.createElement(name)
      for ((attrName, attrValue) <- attributes.toSeq) {
        elem.setAttribute(attrName.name, attrName.toString(attrValue))
      }
      mkNode(elem)
    }

    def updateBlueprint(component : Component, blueprint : Blueprint, optState : Option[Any]) {
      val node = component.mountNode
      val oldBlueprint = component.blueprint
      val oldAttributes = attributesOf(oldBlueprint)
      val attributes = attributesOf(blueprint)
      val elem = node.inner
      val removedAttributes = oldAttributes.attributeNames -- attributes.attributeNames
      for (attrName <- removedAttributes) elem.removeAttribute(attrName.name)
      for ((attrName, attrValue) <- attributes.toSeq) {
        elem.setAttribute(attrName.name, attrName.toString(attrValue))
      }
    }

  }

  case class StringParam(value : String) extends Parameters

  object TEXT extends PrimitiveComponentClass {
    def name = "text"
    def render(component : Component) : DOM.Node = {
      val param : StringParam = component.blueprint()
      val elem = document.createTextNode(param.value)
      mkNode(elem)
    }
    def updateBlueprint(component : Component, blueprint : Blueprint, optState : Option[Any]) {
      val node = component.mountNode
      node.inner.nodeValue = blueprint[StringParam]().value
    }
  }

  object INPUT extends DefaultPrimitiveComponent("input")
  {
    override def updateBlueprint(component : Component, blueprint : Blueprint, optState : Option[Any]) {
      super.updateBlueprint(component, blueprint, optState)
      if (optState.isDefined) updateState(component, optState.get)
    }

    override def updateState(component : Component, state : Any) {
      val s = state.asInstanceOf[String]
      component.mountNode.inner.value = s
    }

    override def getState(component : Component) : Any = {
      val s = component.mountNode.inner.value.asInstanceOf[String]
      s
    }
  }

  private def primitiveClass(name : String) : PrimitiveComponentClass = 
    new DefaultPrimitiveComponent(name)

  def text(value : String) = TEXT(StringParam(value))()

  val DIV = primitiveClass("div")
  val H1 = primitiveClass("h1")
  val H2 = primitiveClass("h2")
  val FORM = primitiveClass("form")

}