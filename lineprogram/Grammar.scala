package lineprogram

/**
 * @author andrew
 */

/**
 * Grammar adapted from Andrew Appel's Modern Compiler implementation..." series.
 * <Stm> ::= <Stm> ";" <Stm> | <id> " := " <Exp> | "print (" <ExpList> ")"
 * <Exp> ::= <id> | <num> | <Exp> <Binop> <Exp> | "(" <Stm> ", " <Exp> ")"
 * <ExpList> ::= <Exp> { ", " <ExpList> }
 * <Binop> ::= "+"| "-" | "*" | "/"
 */

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
