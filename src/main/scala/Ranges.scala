object Ranges {
  def main(args: Array[String]) {
    val range = 1 to 10
    println("1 to 10: " + range.toString())
    
    val until = 1 until 10 
    println("1 until 10: " + until.toString());
    
    val intStep = 1 to 50 by 5
    println ("1 to 50 by 5: " + intStep.toString())
    
    val aThruM = 'A' to 'M'
    println("'A' to 'M': " + aThruM.toString())
    
    val aThruMBy2 = 'A' to 'M' by 2
    println("'A' to 'M' by 2: " + aThruMBy2.toString())
    
    val decimalStep = 1.1 to 2.5 by 0.3
    println("1.1 to 2.5 by 0.3" + decimalStep.toString())
    
    // Works for BigInt and BigDecimal as well
  }
}