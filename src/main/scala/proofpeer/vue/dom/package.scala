package proofpeer.vue

package object dom {

  import scala.scalajs.js

  def document : Node = Node.make(js.Dynamic.global.document)

  def window : Node = Node.make(js.Dynamic.global.window)

  def lookupNode(id : String) : Option[Node] = { 
    val node = document().getElementById(id)
    if (node == null) None else Some(new Node(node))
  }

  def setInterval(f : () => Unit, millis : Int) : Int = {
    window().setInterval(f, millis).asInstanceOf[Int]
  }

  def clearInterval(intervalId : Int) {
    window().clearInterval(intervalId)
  }

  def setTimeout(f : () => Unit, millis : Int) : Int = {
    window().setTimeout(f, millis).asInstanceOf[Int]
  }

  def clearTimeout(intervalId : Int) {
    window().clearTimeout(intervalId)
  }  

  private def primitiveClass(name : String) : PrimitiveComponentClass = 
    new DefaultPrimitiveComponent(name)

  def text(value : String) = TEXT.make(value)
  def rawHtml(value : String) = RAW_HTML.make(value)

  val DIV = primitiveClass("div")
  val H1 = primitiveClass("h1")
  val H2 = primitiveClass("h2")
  val FORM = primitiveClass("form")
  val SPAN = primitiveClass("span")
  val IMG = primitiveClass("img")

  // Attributes

  case object CLASSNAME extends StringAttributeName("classname")
  case object VALUE extends StringAttributeName("value")
  case object PLACEHOLDER extends StringAttributeName("placeholder")
  case object REF extends StringAttributeName("ref")
  case object TYPE extends StringAttributeName("type")
  case object SRC extends StringAttributeName("src")

}