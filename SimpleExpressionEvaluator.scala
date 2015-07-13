/** Grammar:
  *    Expression :: = Term | Term + Factor
  *    Term ::= Factor | Term * Factor
  *	   Factor :: = Constant
  *	   Constant :: = Int
  **/	   
abstract class Expression
case class Add(term: Term, expression: Expression) extends Expression

abstract class Term extends Expression
case class Mul(term: Term, factor: Factor) extends Term

abstract class Factor extends Term
case class Constant(value: Int) extends Factor


object SimpleExpressionEvaluator {
	def main(args: Array[String]) {
		// Define AST for: 2 + 3 * 5		
		val expr = Add(Constant(2), Mul(Constant(3), Constant(5)))
		println(eval(expr))
	}
	
	def eval(expression: Expression): Int  = expression match {
		case Add(x, y) => eval(x) + eval(y)
		case Mul(x, y) => eval(x) * eval(y)
		case Constant(value) => value			
	}
}