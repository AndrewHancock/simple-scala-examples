package expeval

import scala.util.parsing.combinator._;

/**
 * Grammar:
 *    Expression :: = Expression + Term | Expression - Term
 *    Term ::= Factor | Term * Factor | Term / Factor
 * 	   Factor :: = Constant
 * 	   Constant :: = Int
 */
abstract class Expression
case class Add(expression: Expression, term: Term) extends Expression
case class Sub(expression: Expression, term: Term) extends Expression

abstract class Term extends Expression
case class Mul(term: Term, factor: Factor) extends Term
case class Divide(term: Term, factor: Factor) extends Term

abstract class Factor extends Term
case class Constant(value: Int) extends Factor

class ExpParser extends JavaTokenParsers {
  def exp: Parser[List[Expression]] = term ~ rep("+" ~ term | "-" ~ term) ^^ {
    case term ~ termList =>
      for (term1 <- term; comb <- termList; term2 <- comb._2) yield comb._1 match {
        case "+" => Add(term1, term2)
        case "-" => Sub(term1, term2)
      }
  }
  def term: Parser[List[Term]] = factor ~ rep("*" ~ factor | "/ " ~ factor) ^^ {
    case factor1 ~ factorList =>
      if(factorList.isEmpty)
        List(factor1)
       else
        for (comb <- factorList) yield comb._1 match {
          case "*" => Mul(factor1, comb._2)
          case "/" => Divide(factor1, comb._2)
        }
  }
  def factor: Parser[Factor] = floatingPointNumber ^^ {
    case factor => Constant(factor.toInt)
  }
}

object ExpressionEvaluator extends ExpParser {
  def main(args: Array[String]) {
    val input = "2 + 5 * 3 - 7"
    println("Input: " + input);
    val expr = parseAll(exp, input)
    println(expr)
    println(eval(expr.get.head))    
  }

  def eval(expression: Expression): Int = expression match {
    case Add(x, y)       => eval(x) + eval(y)
    case Sub(x, y)       => eval(x) - eval(y)
    case Mul(x, y)       => eval(x) * eval(y)
    case Divide(x, y)    => eval(x) / eval(y)
    case Constant(value) => value
  }
}