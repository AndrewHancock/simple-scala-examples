/** Grammar adapted from Andrew Appel's Modern Compiler implementation..." series.
  * <Stm> ::= <Stm> ";" <Stm> | <id> " := " <Exp> | "print (" <ExpList> ")"
  * <Exp> ::= <id> | <num> | <Exp> <Binop> <Exp> | "(" <Stm> ", " <Exp> ")"
  * <ExpList> ::= <Exp> { ", " <ExpList> }
  * <Binop> ::= " + "| " - " | " * " | " / "
  **/
  
 abstract class Stm
 case class CompoundStm(stm1: Stm, stm2: Stm) extends Stm
 case class AssignStm(id: String, exp: Exp) extends Stm
 case class PrintStm(expList: ExpList) extends Stm
 
 abstract class Exp
 case class IdExp(id: String) extends Exp
 case class NumExp(num: Int) extends Exp
 case class OpExp(exp1: Exp, op: Binop, exp2: Exp) extends Exp
 case class EseqExp(stm: Stm, exp: Exp) extends Exp
 
 abstract class ExpList
 case class PairExpList(exp: Exp, expList: ExpList) extends ExpList
 case class LastExpList(exp: Exp) extends ExpList
 
 abstract class Binop
 case class Plus() extends Binop
 case class Minus() extends Binop
 case class Times() extends Binop
 case class Div() extends Binop
 
 object StraightLineProgram {
	def stmStr(stm: Stm): String = stm match {
		case CompoundStm(stm1, stm2) => stmStr(stm1) + " ; " + stmStr(stm2)	
		case AssignStm(id, exp) => id + " := " + expStr(exp)
		case PrintStm(expList) => "print (" + listStr(expList) + ")"
	}
	
	def expStr(exp: Exp): String = exp match {		
		case IdExp(id) => id
		case NumExp(num) => num.toString()
		case OpExp(exp1, op, exp2) => expStr(exp1) + " " + opStr(op) + " " + expStr(exp2)
		case EseqExp(stm, exp) => "(" + stmStr(stm) + ", " + expStr(exp) + ")"
	}
	
	def listStr(expList: ExpList): String = expList match {		
		case PairExpList(exp, expList) => expStr(exp) + ", " + listStr(expList) 
		case LastExpList(exp) => expStr(exp)
	}
	
	def opStr(op: Binop): String = op match {		
		case Plus() => "+"
		case Minus() => "-"
		case Times() => "*"
		case Div() => "/"		
	}
	
	def main(args: Array[String]) {
		// a := 5+3; b := (print(a, a-1), 10*a); print(b)
		val prog = CompoundStm(
			//a := 5+3;
			AssignStm("a", OpExp(NumExp(5), Plus(), NumExp(3))),
			CompoundStm(
				//b := (print(a, a-1), 10*a);
				AssignStm("b", EseqExp(
					PrintStm(PairExpList(IdExp("a"), 
						LastExpList(OpExp(IdExp("a"), Minus(), NumExp(1))))),
					OpExp(NumExp(10), Times(), IdExp("a")))),
				PrintStm(LastExpList(IdExp("b")))))				
		
		println(stmStr(prog))
	}
}