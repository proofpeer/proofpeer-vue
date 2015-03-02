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

  def runLater(f : () => Unit) : Int = setTimeout(f, 0)

  def clearTimeout(intervalId : Int) {
    window().clearTimeout(intervalId)
  }  

  def getScrollbarWidth() : Int = {
    val outer = document().createElement("div")
    outer.style.visibility = "hidden"
    outer.style.width = "100px"
    outer.style.msOverflowStyle = "scrollbar" // needed for WinJS apps
    document().body.appendChild(outer)
    val widthNoScroll = outer.offsetWidth
    outer.style.overflow = "scroll"
    val inner = document().createElement("div")
    inner.style.width = "100%"
    outer.appendChild(inner)       
    val widthWithScroll = inner.offsetWidth
    outer.parentNode.removeChild(outer)
    return (widthNoScroll - widthWithScroll).asInstanceOf[Int]
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
  val BUTTON = primitiveClass("button")
  val LINK = primitiveClass("a")

  // Attributes

  case object CLASS extends StringAttributeName("class")
  case object VALUE extends StringAttributeName("value")
  case object PLACEHOLDER extends StringAttributeName("placeholder")
  case object REF extends StringAttributeName("ref")
  case object HREF extends StringAttributeName("href")
  case object TYPE extends StringAttributeName("type")
  case object SRC extends StringAttributeName("src")
  case object ROWS extends IntAttributeName("rows")
  case object SPELLCHECK extends BoolAttributeName("spellcheck")
  case object DISABLED extends BoolAttributeName("disabled")

}