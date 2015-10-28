case class Point( x: Double, y: Double) {
  def shift(deltaX: Double, deltaY: Double) =
  {
    copy(x + deltaX, y + deltaY)
  } 
}

object Class {
  def main(args: Array[String]) = 
  {
    val p = Point(2.0, 3.5);
    println("p: " + p.toString)
    
    val shiftedP = p.shift(1.4, -0.8);
    println("p.shift(1.4, -0.8): " + shiftedP.toString)
    
    val copiedP = p.copy(y = 10.0)
    println("p.copy(y = 10.0): " + copiedP.toString())
    
  }
}