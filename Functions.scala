
object Functions {
	// Note omissiong of brackets, one line definition with =
	def add(x : Int, y : Int) = x + y	
	
	// Function takes high-order function as param 1
	def binOp(f:(Int, Int) => Int, x: Int, y: Int) = f(x, y)
	
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
	}
}