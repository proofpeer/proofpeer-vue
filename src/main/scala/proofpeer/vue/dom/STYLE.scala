package proofpeer.vue.dom

import proofpeer.vue._
import proofpeer.general._
import proofpeer.general.StringUtils._

case object STYLE extends AttributeName[CaseInsensitiveMap[String]] {
  val name = "style"
  def read(value : Any) = {
    value match {
      case s : String => None
        var styles : CaseInsensitiveMap[String] = CaseInsensitiveMap()
        for (style <- split_nonempty(s, ";")) {
          val seq = split(style, ":")
          if (seq.size != 2) throw new RuntimeException("invalid style '"+style+"'")
          styles = styles.put(seq(0).trim, seq(1)) 
        }
        Some(styles)
      case styles : CaseInsensitiveMap[_] =>
        Some(styles.asInstanceOf[CaseInsensitiveMap[String]])
      case _ => None
    }
  }
  override def toString(value : Any) : String = {
    var result : String = ""
    for ((k, v) <- value.asInstanceOf[CaseInsensitiveMap[String]].toSeq) {
      result = result + k.toString + ":" + v + ";"
    }
    result
  }
  override def merge(value1 : Any, value2 : Any) : Any = {
    val u = value1.asInstanceOf[CaseInsensitiveMap[String]]
    val v = value2.asInstanceOf[CaseInsensitiveMap[String]]
    u ++ v
  }
}
