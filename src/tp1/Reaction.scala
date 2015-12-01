package tp1

/**
 * danglotb
 */
class Reaction(r : Map[String, Int], p : Map[String, Int], s : Double) {
  
  def reactants = r
  
  def products = p
  
  def speed = s
  
  def computeH(s : Map[String,Int]) : Double = {
    r.keys.foldLeft(1.0) {
      case (acc,key) => acc * (combine(s(key), r(key)))
    }
  }
  
  private def combine(n : Int, k : Int) : Int = {
    if (n >= k) 
      (factorial(n,1) / (factorial(k,1) * factorial(n-k,1))).toInt
    else 
        0
  }
    
  
  private def factorial(i : Int, acc : BigInt) : BigInt = {
    if (i <= 1)
      acc
    else
      factorial(i-1,acc*i)
  }
  
}