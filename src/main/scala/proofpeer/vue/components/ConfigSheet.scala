package proofpeer.vue.components

import proofpeer.vue._

case class FontStyle(
  val family : String,
  val size : Int,
  val lineHeight : Int, // this must be a multiple of ConfigSheet.baselineHeight
  val paddingTop : Int, // this must be chosen such that the font rests on the baseline
  val style : String,
  val weight : String
) {
  
  override def toString : String = asStyle+"padding-top:"+paddingTop+"px;" 

  def asStyle : String =  
    "font-family:"+family+";font-size:"+size+"px;line-height:"+lineHeight+"px;"+
    "font-style:"+style+";font-weight:"+weight+";" 
  
  val asAttribute : Any = proofpeer.vue.dom.STYLE.read(this.toString)
}

case class ConfigSheet(
  val baselineHeight : Int,
  val gutterWidth : Int, // usually baselineHeight / 2
  val smallStyle : FontStyle,
  val bodyStyle : FontStyle,
  val leadStyle : FontStyle,
  val supertitleStyle : FontStyle,
  val titleStyle : FontStyle,
  val subtitleStyle : FontStyle 
) 

object ConfigSheet {

  private val ff = "Arial, Helvetica, sans-serif"
  private val defaultConfigSheet =
    ConfigSheet(
      24,
      12,
      FontStyle(ff, 16, 24, 0, "normal", "normal"),
      FontStyle(ff, 16, 24, 0, "normal", "normal"),
      FontStyle(ff, 20, 24, 0, "normal", "normal"),
      FontStyle(ff, 60, 72, 0, "normal", "bold"),
      FontStyle(ff, 40, 48, 0, "normal", "bold"),
      FontStyle(ff, 32, 48, 0, "normal", "bold")    
    )

  private var applicationConfigSheet : ConfigSheet = defaultConfigSheet

  def set(configSheet : ConfigSheet) {
    applicationConfigSheet = configSheet
  }

  def apply() : ConfigSheet = {
    applicationConfigSheet
  }  
  
}

object FontStyle {
  import proofpeer.vue.dom._

  private val testStr = " !\"\\#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[]^_`abcdefghijklmnopqrstuvwxyz{|}~¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿŒœŠšŸƒˆ˜ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩαβγδεζηθικλμνξοπρςστυφχψωϑϒϖ–—‘’‚“”„†‡•…‰′″‹›‾⁄€ℑ℘ℜ™ℵ←↑→↓↔↵⇐⇑⇒⇓⇔∀∂∃∅∇∈∉∋∏∑−∗√∝∞∠∧∨∩∪∫∴∼≅≈≠≡≤≥⊂⊃⊄⊆⊇⊕⊗⊥⋅⌈⌉⌊⌋〈〉◊♠♣♥♦"


  def isInitialized(fs : FontStyle) : Boolean = {
    val fontFamily = fs.family
    val fontWeight = fs.weight
    val fontStyle = fs.style
    val node = Node.make(document().body)
    val requested = FontStyle(fontFamily+",serif", 40, 40, 0, fontStyle, fontWeight)
    val requestedDiv = DIV(STYLE -> requested.toString)(text(testStr))
    val fallback = FontStyle("serif", 40, 40, 0, fontStyle, fontWeight)
    val fallbackDiv = DIV(STYLE -> fallback.toString)(text(testStr))
    val (w1, h1) = RenderTarget.measure(node, requestedDiv)
    val (w2, h2) = RenderTarget.measure(node, fallbackDiv)
    w1 != w2 || h1 != h2      
  }

  def isInitialized(cs : ConfigSheet) : Boolean = {
    isInitialized(cs.smallStyle) &&
    isInitialized(cs.bodyStyle) &&
    isInitialized(cs.leadStyle) &&
    isInitialized(cs.supertitleStyle) &&
    isInitialized(cs.titleStyle) &&
    isInitialized(cs.subtitleStyle)
  }  
}

object FONT_STYLE extends CustomAttributeName[FontStyle]("fontstyle")

object COPY extends CustomComponentClass {

  def render(parentNode : dom.Node, c : CustomComponent) : Blueprint = {
    import proofpeer.vue.dom._
    val fs = c.attributes.get(FONT_STYLE) match {
      case None => ConfigSheet().bodyStyle
      case Some(fs) => fs 
    }  
    DIV(c, STYLE -> fs.toString)(c.children : _*)
  }

}