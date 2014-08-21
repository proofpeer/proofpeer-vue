package proofpeer.vue.dom

import proofpeer.vue._

object TEXT extends PrimitiveComponentClass {

  object VALUE extends CustomAttributeName[String]("value")

  def name = "text"

  def render(component : Component) : Node = {
    val param = component.attributes(VALUE)
    val elem = document().createTextNode(param)
    Node.make(elem)
  }
  def updateBlueprint(component : Component, blueprint : Blueprint, optState : Option[Any]) {
    val node = component.mountNode
    node().nodeValue = blueprint.attributes(VALUE)
  }
  def make(value : String) : Blueprint = TEXT(VALUE -> value)()
}
