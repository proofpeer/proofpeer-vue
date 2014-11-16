package proofpeer.vue.dom

import scala.scalajs.js
import proofpeer.general._

object Styling {

  sealed trait Handle {
    def tryAddRule(rule : String) : Boolean
    def addRule(rule : String)
    def numRules : Int
    def remove()
  }

  private class HandleImpl (styleElement : js.Dynamic) extends Handle {
    private var element : js.Dynamic = styleElement
    def numRules = element.sheet.cssRules.length.asInstanceOf[Int]
    def tryAddRule(rule : String) : Boolean = {
      try {
        element.sheet.insertRule(rule, numRules)
        true
      } catch {
        case _ : Exception => false
      }
    }
    def addRule(rule : String) {
      element.sheet.insertRule(rule, numRules)
    }
    def remove() {
      if (element != null) {
        document().head.removeChild(element)
        element = null
      }
    }
  }

  /** Adds the given style rules to the document without any processing of the rules. */
  def addRules(rules : String*) : Handle = {
    val doc = document()
    val styleElement = doc.createElement("style")
    styleElement.appendChild(doc.createTextNode(""))
    doc.head.appendChild(styleElement)
    val styleSheet = styleElement.sheet
    val handle = new HandleImpl(styleElement)
    for (rule <- rules) {
      if (!handle.tryAddRule(rule)) {
        handle.remove()
        throw new RuntimeException("Invalid styling rule: '" + rule + "'")
      }
    }
    handle
  }

  private def readAnimationStyles(styles : Any*) : String = {
    var m : CaseInsensitiveMap[String] = CaseInsensitiveMap()
    for (s <- styles) m = m ++ STYLE.read(s).get
    var vendorspecific : CaseInsensitiveMap[String] = CaseInsensitiveMap()
    for ((name, value) <- m.toSeq) {
      if (name.toLowerCase.startsWith("animation-")) 
        vendorspecific = vendorspecific.put("-webkit-"+name, value)
    }
    STYLE.toString(m ++ vendorspecific)
  }

  /** Adds a css class to the document which describes an animation. 
    * @param classname the name of the class, which will also be the name of the animation
    * @param timing styles describing the timing behaviour of the class
    * @param frames the keyframes, where each frame is a pair (percentage, styles)
    */
  def addAnimation(classname : String, timing : Any, frames : (Int, Any)*) : Handle = {
    var done = false
    var handle : Handle = addRules()
    try {
      handle.addRule("." + classname + "{" + readAnimationStyles(timing, "animation-name:" + classname) + "}")
      val framerules = new StringBuilder("")
      for ((p, fs) <- frames) {
        if (p < 0 || p > 100) throw new RuntimeException("invalid frame percentage: " + p)
        val framestyles = readAnimationStyles(fs) 
        framerules.append("" + p + "% {" + framestyles + "}")
      }
      val framerule = "keyframes " + classname + "{" + framerules.toString + "}"
      val ok_standard = handle.tryAddRule("@" + framerule)
      val ok_webkit = handle.tryAddRule("@-webkit-" + framerule)
      if (!ok_standard && !ok_webkit) throw new RuntimeException("invalid keyframes rule: @" + framerule)
      done = true     
      handle
    } finally {
      if (!done) handle.remove()
    }
  }

}