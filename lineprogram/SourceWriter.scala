package lineprogram

object SourceWriter {

  def stmStr(stm: Stm): String = stm match {
    case CompoundStm(stm1, stm2) => stmStr(stm1) + " ; " + stmStr(stm2)
    case AssignStm(id, exp)      => id + " := " + expStr(exp)
    case PrintStm(expList)       => "print (" + listStr(expList) + ")"
  }

  def expStr(exp: Exp): String = exp match {
    case IdExp(id)             => id
    case NumExp(num)           => num.toString()
    case OpExp(exp1, op, exp2) => expStr(exp1) + opStr(op) + expStr(exp2)
    case EseqExp(stm, exp)     => "(" + stmStr(stm) + ", " + expStr(exp) + ")"
  }

  def listStr(expList: ExpList): String = expList match {
    case PairExpList(exp, expList) => expStr(exp) + ", " + listStr(expList)
    case LastExpList(exp)          => expStr(exp)
  }

  def opStr(op: Binop): String = op match {
    case Plus()  => "+"
    case Minus() => "-"
    case Times() => "*"
    case Div()   => "/"
  }
}