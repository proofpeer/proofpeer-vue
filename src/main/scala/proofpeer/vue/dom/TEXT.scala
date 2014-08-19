package proofpeer.vue.dom

import proofpeer.vue._

object TEXT extends PrimitiveComponentClass {
  def name = "text"
  def render(component : Component) : Node = {
    val param : String = component.blueprint.attribute
    val elem = document.createTextNode(param)
    Node.make(elem)
  }
  def updateBlueprint(component : Component, blueprint : Blueprint, optState : Option[Any]) {
    val node = component.mountNode
    node.inner.nodeValue = blueprint.attribute[String]
  }
  def make(value : String) : Blueprint = TEXT(AttributeName.DEFAULT -> value)()
}
