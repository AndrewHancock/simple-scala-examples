package lineprogram

object Interpreter {
  def evalStm(symTable: Map[String, Int], stm: Stm): Map[String, Int] = stm match {
    case CompoundStm(stm1, stm2) => evalStm(evalStm(symTable, stm1), stm2)
    case AssignStm(id, exp)      => symTable + (id -> evalExp(symTable, exp))
    case PrintStm(expList)       => { println(evalList(symTable, expList)); symTable }
  }

  def evalExp(symTable: Map[String, Int], exp: Exp): Int = exp match {
    case IdExp(id)             => symTable(id)
    case NumExp(num)           => num
    case OpExp(exp1, op, exp2) => evalOp(evalExp(symTable, exp1), op, evalExp(symTable, exp2))
    case EseqExp(stm, exp)     => { evalStm(symTable, stm); evalExp(symTable, exp) }
  }

  def evalList(symTable: Map[String, Int], expList: ExpList): String = expList match {
    case PairExpList(exp, expList) => evalExp(symTable, exp).toString() + " " + evalList(symTable, expList)
    case LastExpList(exp)          => evalExp(symTable, exp).toString()
  }

  def evalOp(x: Int, op: Binop, y: Int): Int = op match {
    case Plus()  => x + y
    case Minus() => x - y
    case Times() => x * y
    case Div()   => x * y
  }
}