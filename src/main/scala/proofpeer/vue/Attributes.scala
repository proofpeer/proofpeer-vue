package proofpeer.vue

import proofpeer.general._
import proofpeer.general.StringUtils._

trait AttributeName[+T] {
  def name : String
  def read(value : Any) : Option[T]
  def toString(value : Any) : String = value.toString
}

object AttributeName {

  abstract class StringAttributeName(val name : String) extends AttributeName[String] {
    def read(value : Any) = Some(value.toString)
  }

  abstract class IntAttributeName(val name : String) extends AttributeName[Int] {
    def read(value : Any) = {
      value match {
        case i : Int => Some(i)
        case _ => None
      }
    }
  }  

  abstract class CustomAttributeName[T](val name : String) extends AttributeName[T] {
    def read(value : Any) : Option[T] = {
      Some(value.asInstanceOf[T])
    }
    override def toString(value : Any) : String = null
  }

  case object STYLE extends AttributeName[CaseInsensitiveMap[String]] {
    val name = "style"
    def read(value : Any) = {
      value match {
        case s : String => None
          var styles : CaseInsensitiveMap[String] = CaseInsensitiveMap()
          for (style <- split_nonempty(s, ";")) {
            val seq = split(style, ":")
            if (seq.size != 2) throw new RuntimeException("invalid style '"+style+"'")
            styles = styles.put(seq(0), seq(1)) 
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
  }

  case object DEFAULT extends CustomAttributeName[Any]("default") 
  case object KEY extends CustomAttributeName[Any]("key")

  case object CLASSNAME extends StringAttributeName("classname")
  case object VALUE extends StringAttributeName("value")
  case object PLACEHOLDER extends StringAttributeName("placeholder")
  case object REF extends StringAttributeName("ref")
  case object TYPE extends StringAttributeName("type")
}

trait Attributes {
  def attributeNames : Set[AttributeName[_]]
  def toSeq : Seq[(AttributeName[Any], Any)]
  def get[T](attributeName : AttributeName[T]) : Option[T]
  def apply[T](attributeName : AttributeName[T]) : T = get(attributeName).get
  def apply[T]() : T = this(AttributeName.DEFAULT).asInstanceOf[T]
  def +[T](attribute : (AttributeName[T], T)) : Attributes
  def ++(attributes : Attributes) : Attributes
}

object Attributes {

  private case class Attr(attributes : Map[AttributeName[Any], Any]) extends Attributes {      
    def attributeNames = attributes.keySet
    
    def get[T](attributeName : AttributeName[T]) : Option[T] = {
      attributes.get(attributeName) match {
        case None => None
        case Some(x) => Some(x.asInstanceOf[T])
      }
    }

    def +[T](attribute : (AttributeName[T], T)) : Attributes = {
      Attr(attributes + attribute)
    }

    def toSeq : Seq[(AttributeName[Any], Any)] = {
      attributes.toSeq
    }

    def ++(that : Attributes) : Attributes = {
      val attr = that.asInstanceOf[Attr]
      Attr(attributes ++ attr.attributes)    
    }
  }

  def apply(attributes : (AttributeName[Any], Any)*) : Attributes = {
    var m : Map[AttributeName[Any], Any] = Map()
    for ((name, value) <- attributes) {
      name.read(value) match {
        case Some(v) => m = m + (name -> v)
        case None => throw new RuntimeException("invalid value for attribute "+name.name+": "+value)
      }
    }
    Attr(m)
  }

}
