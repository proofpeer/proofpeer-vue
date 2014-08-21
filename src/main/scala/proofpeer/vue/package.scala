package proofpeer

package object vue {

  def ensure(condition : Boolean, errorMessage : String) {
    if (!condition) throw new RuntimeException(errorMessage)
  }  

  case object KEY extends CustomAttributeName[Any]("key")

}