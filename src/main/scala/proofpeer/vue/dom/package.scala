package proofpeer.vue

package object dom {

  import scala.scalajs.js

  def document : js.Dynamic = js.Dynamic.global.document

  def window : js.Dynamic = js.Dynamic.global.window

  def screen : js.Dynamic = js.Dynamic.global.screen

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

  private def primitiveClass(name : String) : PrimitiveComponentClass = 
    new DefaultPrimitiveComponent(name)

  def text(value : String) = TEXT.make(value)

  val DIV = primitiveClass("div")
  val H1 = primitiveClass("h1")
  val H2 = primitiveClass("h2")
  val FORM = primitiveClass("form")
  val SPAN = primitiveClass("span")

}