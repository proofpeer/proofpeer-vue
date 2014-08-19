package proofpeer.vue.dom

import scala.scalajs.js

class Node(private val node : js.Dynamic) {

  def appendChild(child : Node) {
    node.appendChild(child.node)
  }

  def countChildren : Int = node.childNodes.length.asInstanceOf[Int]

  def insertBefore(child : Node, before : Node) {
    node.insertBefore(child(), before())
  }

  def replace(nodeToReplace : Node) {
    val parent = nodeToReplace.node.parentNode
    parent.replaceChild(node, nodeToReplace.node)
  }

  def removeChild(child : Node) {
    node.removeChild(child())
  }

  def contains(that : Node) : Boolean = {
    node.contains(that()).asInstanceOf[Boolean]
  }

  // returns the active element if there is one contained in this node
  def activeElement : Option[Node] = {
    val a = document().activeElement
    if (a == null) None
    else if (node.contains(a).asInstanceOf[Boolean]) Some(Node.make(a))
    else None
  }

  def apply() : js.Dynamic = node

}

object Node {
  def make(node : js.Dynamic) : Node = new Node(node)
}