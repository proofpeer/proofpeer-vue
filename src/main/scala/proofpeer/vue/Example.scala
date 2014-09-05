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

    def render(parentNode : dom.Node, component : CustomComponent) : Blueprint = {
      val state = component.getLocalState()
      val commentBoxState = state.asInstanceOf[CommentBoxState]
      DIV(Event.OnSubmit -> handler(component))(
        H1()(text("Comments")),
        CommentForm(KEY -> "form")(),
        CommentList(CommentList.COMMENTS -> commentBoxState.comments)()
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

    object COMMENTS extends CustomAttributeName[List[CommentData]]("comments")

    def render(parentNode : dom.Node, component : CustomComponent) : Blueprint = {
      val data = component.attributes(COMMENTS)
      val comments = 
        for (d <- data) 
          yield Comment(Event.OnClick -> this, Comment.PARAMS -> CommentParams(d.author, d.background))(text(d.text))
      DIV(Event.OnClick -> this)(comments : _*)
    }
    def handleEvent(component : Component, event : Event) {
      event.stopPropagation()
      event.preventDefault()
    }
  }

  case class CommentFormState(author : String, comment : String) 
  object CommentForm extends CustomComponentClass {
    def render(parentNode : dom.Node, component : CustomComponent) : Blueprint = {
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

    object PARAMS extends CustomAttributeName[CommentParams]("params")

    def render(parentNode : dom.Node, component : CustomComponent) : Blueprint = {
      val params : CommentParams = component.attributes(PARAMS)
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

  val lorem_ipsum = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec ultricies ex eu nibh mollis, ac lacinia libero blandit. Aenean vulputate tempus nulla in sodales. Proin elit erat, volutpat sed sem vel, vehicula posuere elit. Quisque vitae tortor sed ligula aliquam fermentum. Morbi sit amet volutpat diam. Aenean tincidunt dapibus accumsan. Donec tristique eros eget nulla mattis fermentum. Mauris quis mollis risus. Duis euismod, ligula faucibus blandit iaculis, sapien neque tincidunt orci, a viverra dui tortor scelerisque lacus. Aenean ut nisi aliquam, viverra dui mattis, pulvinar leo. Quisque est turpis, pulvinar ac tortor a, iaculis efficitur eros. Pellentesque habitant morbi tristique senectus et netus et malesuada fames ac turpis egestas. Ut ut euismod dui, ut auctor libero"

  def simpleGrid : Blueprint = {
    import GRID_LAYOUT._
    object SimpleGrid extends CustomComponentClass {
      def render(parentNode : dom.Node, c : CustomComponent) : Blueprint = {
        val dims = c.attributes(DIMS)
        val grid = new GoldenGridSystem(dims.width.get, 18)
        val topelem = SHOW_DIMS(STYLE -> "background-color:blue")()
        val subelem = SHOW_DIMS(STYLE -> "background-color:red")()
        val textelem = DIV(STYLE->"overflow:hidden")(text(lorem_ipsum))
        val numBaselines = dims.height.get / grid.baseline
        val b1 = Absolute(math.round(0.4 * numBaselines).asInstanceOf[Int])
        val b2 = Absolute(b1.baseline + 2)
        val b3 = Absolute(numBaselines - 2)
        val positions : List[Position] = List(
          Position(0, 17, true, true, Absolute(0), b1),
          Position(1, 4, false, false, b2, b3),
          Position(5, 8, false, false, b2, b3),
          Position(9, 12, false, false, b2, b3),
          Position(13, 16, false, false, b2, b3))
        GRID_LAYOUT(c, STYLE -> "background-color:darkgray", GRID -> grid, POSITIONS -> positions,
          SHOW_GRID -> true)(
          topelem, 
          textelem, 
          subelem, 
          textelem,
          textelem)
      }
    }
    SimpleGrid()()
  }

  @JSExport
  def pagecontainer() : Unit = {
    RenderTarget(lookupNode("content").get).render(
      PAGE()(
        simpleGrid
      )
    )
  }

}