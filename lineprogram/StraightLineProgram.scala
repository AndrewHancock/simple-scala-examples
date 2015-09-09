package lineprogram;
 
 object StraightLineProgram extends Parser {
	def main(args: Array[String]) {
		// Hardcoded AST for the following program:
		// a := 5+3; b := (print(a, a-1), 10*a); print(b)
		val prog = CompoundStm(			
			AssignStm("a", OpExp(NumExp(5), Plus(), NumExp(3))),
			CompoundStm(				
				AssignStm("b", EseqExp(
					PrintStm(PairExpList(IdExp("a"), 
						LastExpList(OpExp(IdExp("a"), Minus(), NumExp(1))))),
					OpExp(NumExp(10), Times(), IdExp("a")))),
				PrintStm(LastExpList(IdExp("b")))))				
		
		// Print out he source code for the program
		println(SourceWriter.stmStr(prog))
		
		// Evaluate the program, including side-effects		
		// Should print:
		// 8 7
		// 80
		Interpreter.evalStm(Map[String, Int](), prog)
    
    println("Enter program: ")
    for (ln <- io.Source.stdin.getLines) {
      println("Enter program: ")
      val prog = parseAll(stm, ln).get
      println(prog)
      println(SourceWriter.stmStr(prog))
      Interpreter.evalStm(Map[String, Int](), prog)      
    }
    
	}
}