
import scala.annotation.tailrec

object Functions {
	// Note missing of brackets, one line definition with =
	def add(x : Int, y : Int) = x + y	
	
	// Function takes high-order function as param 1
	def binOp(f:(Int, Int) => Int, x: Int, y: Int) = f(x, y)
  
  def factorial(x: Int) = {
    @tailrec
    def fact(x: Int, accumulator : Int) : Long =
    {
      if(x < 1) accumulator
      else fact(x - 1, accumulator * x)
    }
  fact(x, 1)
}
  
	
	def main(args: Array[String]) {
		// Standard function call
		println(add(1, 2))
		
		// Pass add function as first param
		println(binOp(add, 4, 5))
		
		//Anonymous function
		println(binOp((x: Int, y: Int) => x * y, 3, 5))
		
		val num = 15;
		val closure =  (x: Int, y: Int) => x * y + num
		//Anonymous function with closure over num
		println(binOp(closure, 2, 3))		
    
    // Partial functions - Do not define all possibilities
    val pf1 : PartialFunction[Any, String] = {
      case s: String => "YES"
    }
    
    val pf2 : PartialFunction[Any, String] = {
      case d: Double => "YES"  
    }
      
    val pf3 = pf1 orElse pf2    
    println("pf3: "+ pf3(1.2));
    
    val factorials = (1 to 5) foreach (i => println( factorial(i)) );    
        
	}
}