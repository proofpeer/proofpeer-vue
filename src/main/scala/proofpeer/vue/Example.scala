package proofpeer.vue

import scala.scalajs.js
import js.annotation.JSExport

@JSExport("proofpeer.vue.Example")
object Example {

  import dom._

  def style(s : String) : Attributes = {
    Attributes(STYLE -> s)
  }

  case class CommentData(author : String, text : String, background : String)

  object Datastore {

    private var data : List[CommentData] = 
      List(
        CommentData("Pete Hunt", "This is one comment.", "lightgray"),
        CommentData("Jordan Walke", "This is *another* comment.", "white"))

    def load() : List[CommentData] = {
      val result = data
      val i = data.size + 1
      val color = if (i % 2 == 0) "lightgray" else "white"
      data = CommentData("User "+i, "This is comment nr. "+i, color) :: data
      return result
    }

    def add(author : String, comment : String) {
      data = CommentData(author, comment, "yellow") :: data
    }

  }

  case class CommentBoxState(intervalId : Int, comments : List[CommentData]) {
    def setComments(comments : List[CommentData]) : CommentBoxState = {
      CommentBoxState(intervalId, comments)
    }
  }

  object CommentBox extends CustomComponentClass {

    def load(component : Component) {
      val state = component.getState[CommentBoxState]
      val updatedState = state.setComments(Datastore.load())
      component.setState(updatedState)

    }

    override def componentWillMount(component : CustomComponent) {
      val intervalId = dom.setInterval(() => { load(component) }, 5000)
      component.setState(CommentBoxState(intervalId, Datastore.load()))      
    }

    override def componentWillUnmount(component : CustomComponent) {
      dom.clearInterval(component.getState[CommentBoxState].intervalId)
    }

    def render(component : CustomComponent) : Blueprint = {
      val state = component.getLocalState()
      val commentBoxState = state.asInstanceOf[CommentBoxState]
      DIV(Event.OnSubmit -> handler(component))(
        H1()(text("Comments")),
        CommentForm(KEY -> "form")(),
        CommentList(DEFAULT -> commentBoxState.comments)()
      )
    }

    def handler(component : Component) : Event.Handler = {
      object h extends Event.Handler {
        def handleEvent(srcComponent : Component, event : Event) {
          val form = component("form")
          val formState : CommentFormState = form.getState
          form.setState(CommentFormState("", ""))
          Datastore.add(formState.author, formState.comment)
          load(component)
          event.preventDefault()
        }
      }
      h
    }
  }

  object CommentList extends CustomComponentClass with Event.Handler {
    def render(component : CustomComponent) : Blueprint = {
      val data = component.attribute[List[CommentData]]
      val comments = 
        for (d <- data) 
          yield Comment(Event.OnClick -> this, DEFAULT -> CommentParams(d.author, d.background))(text(d.text))
      DIV(Event.OnClick -> this)(comments : _*)
    }
    def handleEvent(component : Component, event : Event) {
      event.stopPropagation()
      event.preventDefault()
    }
  }

  case class CommentFormState(author : String, comment : String) 
  object CommentForm extends CustomComponentClass {
    def render(component : CustomComponent) : Blueprint = {
      FORM()(
        INPUT(KEY -> "author", TYPE -> "text", PLACEHOLDER -> "Your name")(),
        INPUT(KEY -> "comment", TYPE -> "text", PLACEHOLDER -> "Say something")(),
        INPUT(TYPE->"submit", VALUE->"Post")())
    }
    override def setState(component : CustomComponent, s : Any) {
      val state = s.asInstanceOf[CommentFormState]
      component("author").setState(state.author)
      component("comment").setState(state.comment)
    }
    override def getState(component : CustomComponent) : CommentFormState = {
      val author = component("author").getState[String]
      val comment = component("comment").getState[String]
      CommentFormState(author, comment)
    }
  }

  case class CommentParams(author : String, background : String)
  object Comment extends CustomComponentClass {
    def render(component : CustomComponent) : Blueprint = {
      val params : CommentParams = component.attribute
      DIV(style("background-color:" + params.background))(
        (H2()(text(params.author)) +:
        component.children) : _*
      )
    }
  }

  @JSExport
  def run() : Unit = {
    RenderTarget(lookupNode("content").get).render(CommentBox()())
  }

  import proofpeer.vue.components._

  def simpleRaster : Blueprint = {
    val elem1 = SHOW_DIMS(STYLE -> "background-color:blue")()
    val pos1 = RasterPosition(Percentage(0), Percentage(0), Percentage(0.2), Percentage(1))
    val elem2 = SHOW_DIMS(STYLE -> "background-color:gray")()
    val pos2 = RasterPosition(Coordinate(0.2, 1), Percentage(0), Percentage(1), Percentage(1))
    RASTER_LAYOUT(DEFAULT -> List(pos1, pos2))(elem1, elem2)
  }

  @JSExport
  def pagecontainer() : Unit = {
    RenderTarget(lookupNode("content").get).render(
      PAGE()(
        //SHOW_DIMS(STYLE->"background-color:blue")()
        simpleRaster
      )
    )
  }

}