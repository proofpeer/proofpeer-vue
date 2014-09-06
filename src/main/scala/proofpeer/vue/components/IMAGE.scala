package proofpeer.vue.components

import proofpeer.vue._
import dom._

object IMAGE extends CustomComponentClass {

  case class Source(url : String, width : Int, height : Int)

  case object SOURCES extends CustomAttributeName[List[Source]]("sources")

  def render(parentNode : dom.Node, component : CustomComponent) : Blueprint = {
    val sources = component.attributes(SOURCES)
    ensure(sources.size >= 1, "at least one image source expected")
    val dims = component.attributes(DIMS)
    val pixelRatio = dims.pixelRatio.get
    var minWidth = dims.minimalWidth * pixelRatio
    var minHeight = dims.minimalHeight * pixelRatio
    var maxWidth = dims.maximalWidth match { case None => None case Some(w) => Some(w * pixelRatio) }
    var maxHeight = dims.maximalHeight match { case None => None case Some(h) => Some(h * pixelRatio) }

    def maxScaleFactor(width : Int, height : Int) : Double = {
      (maxWidth, maxHeight) match {
        case (None, None) => 1.0
        case (Some(maxWidth), None) => maxWidth / width
        case (None, Some(maxHeight)) => maxHeight / height
        case (Some(maxWidth), Some(maxHeight)) =>
          val sw = maxWidth / width
          val sh = maxHeight / height
          if (sw < sh) sw else sh
      }
    }

    def minScaleFactor(width : Int, height : Int) : Double = {
      val sw = minWidth / width
      val sh = minHeight / height
      if (sw > sh) sw else sh
    }

    def scaleFactor(src : Source) : Double = {
      val sMin = minScaleFactor(src.width, src.height)
      val sMax = maxScaleFactor(src.width, src.height)
      if (sMin < sMax) sMax else sMin
    }

    def score(s : Double) : Double = {
      if (s < 1) 1/s
      else if (s < 1.2) s 
      else s * 1000
    }

    def chooseBetterSource(u : Source, v : Source) : Source = {
      val uScore = score(scaleFactor(u))
      val vScore = score(scaleFactor(v))
      if (uScore < vScore) u
      else if (uScore > vScore) v
      else u
    }

    var source = sources.head
    for (s <- sources.tail) source = chooseBetterSource(source, s)
    
    val s = scaleFactor(source) 
    val sourceWidth = math.round(s / pixelRatio * source.width)
    val sourceHeight = math.round(s / pixelRatio * source.height)
    val width = 
      dims.maximalWidth match {
        case None => sourceWidth
        case Some(w) => if (w < sourceWidth) w else sourceWidth
      }
    val height = 
      dims.maximalHeight match {
        case None => sourceHeight
        case Some(h) => if (h < sourceHeight) h else sourceHeight
      }
    val divStyle = "width:"+width+"px;height:"+height+"px;overflow:hidden"
    val left = (width - sourceWidth) / 2
    val top = (height - sourceHeight) / 2
    val imgStyle = "position:absolute;top:"+top+"px;left:"+left+"px;width:"+sourceWidth+"px;height:"+sourceHeight+"px"
    DIV(component, STYLE -> divStyle)(
      IMG(STYLE -> imgStyle, SRC -> source.url)()
    )
  }

}