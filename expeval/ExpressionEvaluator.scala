package expeval
/** Grammar:
  *    Expression :: = Expression + Term | Expression - Term
  *    Term ::= Factor | Term * Factor | Term / Factor
  *	   Factor :: = Constant
  *	   Constant :: = Int
  **/	   
abstract class Expression
case class Add(expression: Expression, term: Term) extends Expression
case class Sub(expression: Expression, term: Term) extends Expression

abstract class Term extends Expression
case class Mul(term: Term, factor: Factor) extends Term
case class Divide(term: Term, factor: Factor) extends Term

abstract class Factor extends Term
case class Constant(value: Int) extends Factor


object ExpressionEvaluator {
	def main(args: Array[String]) {
		// Define AST for: 2 + 3 * 5		
		val expr = Add(Constant(2), Mul(Constant(3), Constant(5)))
		println(eval(expr))
		
		// define AST for: 2 + 3 * 5 - 4 / 2
		val expr2 = Sub(Add(Constant(2), Mul(Constant(3), Constant(5))), Divide(Constant(4), Constant(2)))
		println(eval(expr2))
	}
	
	def eval(expression: Expression): Int  = expression match {
		case Add(x, y) => eval(x) + eval(y)		 
		case Sub(x, y) => eval(x) - eval(y)
		case Mul(x, y) => eval(x) * eval(y)
		case Divide(x, y) => eval(x) / eval(y)
		case Constant(value) => value			
	}
}