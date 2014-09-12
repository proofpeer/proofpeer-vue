package proofpeer.vue.dom

import proofpeer.vue._

object TEXT extends PrimitiveComponentClass {

  object VALUE extends CustomAttributeName[String]("value")

  def name = "text"

  def render(parentNode : dom.Node, component : Component) : Node = {
    val param = component.attributes(VALUE)
    val elem = document().createTextNode(param)
    Node.make(elem)
  }
  def updateBlueprint(parentNode : dom.Node, component : Component, blueprint : Blueprint, optState : Option[Any]) {
    val node = component.mountNode
    node().nodeValue = blueprint.attributes(VALUE)
  }
  def make(value : String) : Blueprint = TEXT(VALUE -> value)()

}

object RAW_HTML extends PrimitiveComponentClass {

  object VALUE extends CustomAttributeName[String]("value")

  def name = "rawhtml"

  def render(parentNode : dom.Node, component : Component) : Node = {
    val elem = document().createElement("DIV")
    for ((attrName, attrValue) <- component.attributes.toSeq) {
      val value = attrName.toString(attrValue)
      if (value != null) elem.setAttribute(attrName.name, value)          
    }
    elem.innerHTML = component.attributes(VALUE)
    Node.make(elem)
  }
  def updateBlueprint(parentNode : dom.Node, component : Component, blueprint : Blueprint, optState : Option[Any]) {
    val node = component.mountNode
    val oldBlueprint = component.blueprint
    val oldAttributes = oldBlueprint.attributes
    val attributes = blueprint.attributes
    val elem = node()
    val removedAttributes = oldAttributes.attributeNames -- attributes.attributeNames
    for (attrName <- removedAttributes) elem.removeAttribute(attrName.name)
    for ((attrName, attrValue) <- attributes.toSeq) {
      val value = attrName.toString(attrValue)
      if (value != null) elem.setAttribute(attrName.name, value)
    }
    elem.innerHTML = blueprint.attributes(VALUE)
  }
  def make(value : String) : Blueprint = RAW_HTML(VALUE -> value)()

}
