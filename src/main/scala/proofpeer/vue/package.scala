package proofpeer

package object vue {

  def ensure(condition : Boolean, errorMessage : String) {
    if (!condition) throw new RuntimeException(errorMessage)
  }  

  case object DEFAULT extends CustomAttributeName[Any]("default") 
  case object KEY extends CustomAttributeName[Any]("key")

}