package lineprogram

import scala.util.parsing.combinator._;
    /**
   * Grammar adapted from Andrew Appel's Modern Compiler implementation..." series.
   * <Stm> ::= <Stm> ";" <Stm> | <id> " := " <Exp> | "print (" <ExpList> ")"
   * <Exp> ::= <id> | <num> | <Exp> <Binop> <Exp> | "(" <Stm> ", " <Exp> ")"
   * <ExpList> ::= <Exp> { ", " <ExpList> }
   * <Binop> ::= "+"| "-" | "*" | "/"
   */
  
class Parser extends JavaTokenParsers with PackratParsers{

   lazy val stm: PackratParser[Stm] =
   stm~";"~stm ^^ { case stm1~";"~stm2 => CompoundStm(stm1, stm2) } |   
  "print"~"(" ~> explist <~ ")" ^^ { (explist) => PrintStm(explist) } |
   id~":="~exp ^^ { case id~":="~exp => AssignStm(id.id, exp)}
  
  lazy val exp: PackratParser[Exp] =    
    exp~binop~exp ^^ {case exp1~binop~exp2 => OpExp(exp1, binop, exp2) }|
    id |
    """[0-9]+""".r ^^ {(num) => NumExp(num.toInt)} |
    "("~> stm~","~exp <~")" ^^ {case stm~","~exp => EseqExp(stm, exp)} 
    
  lazy val explist: PackratParser[ExpList] =    
    exp~","~explist ^^ { case exp~","~explist => PairExpList(exp, explist) } |
    exp ^^ {case exp => LastExpList(exp)}    
  
  lazy val id =
    """[a-zA-Z][0-9a-zA-Z]*""".r ^^ { (id) => IdExp(id)}
  
  lazy val binop = 
    """\+""".r ^^ { (_) => Plus() } |
    """-""".r ^^ { (_) => Minus() } |
    """\*""".r ^^ { (_) => Times() } |
    """/""".r ^^ { (_) => Div() }  
}