package proofpeer.vue.dom

import proofpeer.vue._

object INPUT extends DefaultPrimitiveComponent("input")
{

  override def updateBlueprint(parentNode : dom.Node, component : Component, blueprint : Blueprint, optState : Option[Any]) {
    super.updateBlueprint(parentNode, component, blueprint, optState)
    if (optState.isDefined) updateState(component, optState.get)
  }

  override def updateState(component : Component, state : Any) {
    val s = state.asInstanceOf[String]
    component.mountNode().value = s
  }

  override def getState(component : Component) : Any = {
    val s = component.mountNode().value.asInstanceOf[String]
    s
  }

}
