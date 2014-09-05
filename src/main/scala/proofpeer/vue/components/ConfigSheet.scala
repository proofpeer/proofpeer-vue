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
  
  override def toString : String = 
    "font-family:"+family+";font-size:"+size+"px;line-height:"+lineHeight+"px;"+
    "padding-top:"+paddingTop+"px;font-style:"+style+";font-weight:"+weight+";"  
  
  val asAttribute : Any = proofpeer.vue.dom.STYLE.read(this.toString)
}

case class ConfigSheet(
  val baselineHeight : Int,
  val gutterWidth : Int, // usually baselineHeight / 2
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