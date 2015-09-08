package expeval

import scala.util.parsing.combinator._;
import scala.io.Source;

/**
 * Grammar:
 *    Expression =  Term "+" Expression | Term "-" Expression | Term
 *    Term = Factor "*" Term | Factor "/" Term | Factor
 * 	  Factor = Constant
 * 	  Constant = Int
 */
abstract class Expression
case class Add(term: Term, exp: Expression) extends Expression
case class Sub(term: Term, exp: Expression) extends Expression

abstract class Term extends Expression
case class Mul(factor : Factor, term : Term) extends Term
case class Divide(factor : Factor, term : Term) extends Term

abstract class Factor extends Term
case class Constant(value: Double) extends Factor

class ExpParser extends JavaTokenParsers {
  def exp: Parser[Expression] = 
    (term ~ "+" ~ exp) ^^ { case term ~ "+" ~ exp => Add(term, exp) } |
    (term ~ "-" ~ exp) ^^ { case term ~ "-" ~ exp => Sub(term, exp) } |
    term ^^ { case term => term } 

  def term: Parser[Term] = 
    factor ~ "*" ~ term ^^ { case factor ~ "*" ~ term => Mul(factor, term) } |
    factor ~ "/" ~ term ^^ { case factor ~ "/" ~ term => Divide(factor, term) } |
    factor ^^ { case factor => factor } 

  def factor: Parser[Factor] = floatingPointNumber ^^ {
    case factor => Constant(factor.toDouble)
  }
}

object ExpressionEvaluator extends ExpParser {
  def main(args: Array[String]) {    
    for (ln <- io.Source.stdin.getLines) {
      val expr = parseAll(exp, ln)
      println(expr)
      println(eval(expr.get))      
    }
  }

  def eval(expression: Expression): Double = expression match {
    case Add(x, y)       => eval(x) + eval(y)
    case Sub(x, y)       => eval(x) - eval(y)
    case Mul(x, y)       => eval(x) * eval(y)
    case Divide(x, y)    => eval(x) / eval(y)
    case Constant(value) => value
  }
}