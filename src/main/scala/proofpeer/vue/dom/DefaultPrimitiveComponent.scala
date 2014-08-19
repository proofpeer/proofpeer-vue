package proofpeer.vue.dom

import proofpeer.vue._

class DefaultPrimitiveComponent(val name : String) 
  extends PrimitiveComponentClass
{

  def render(component : Component) : dom.Node = {
    val elem = document.createElement(name)
    for ((attrName, attrValue) <- component.blueprint.attributes.toSeq) {
      val value = attrName.toString(attrValue)
      if (value != null) elem.setAttribute(attrName.name, value)
    }
    Node.make(elem)
  }

  def updateBlueprint(component : Component, blueprint : Blueprint, optState : Option[Any]) {
    val node = component.mountNode
    val oldBlueprint = component.blueprint
    val oldAttributes = oldBlueprint.attributes
    val attributes = blueprint.attributes
    val elem = node.inner
    val removedAttributes = oldAttributes.attributeNames -- attributes.attributeNames
    for (attrName <- removedAttributes) elem.removeAttribute(attrName.name)
    for ((attrName, attrValue) <- attributes.toSeq) {
      val value = attrName.toString(attrValue)
      if (value != null) elem.setAttribute(attrName.name, value)
    }
  }

}
