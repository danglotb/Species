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
      case (acc,key) => acc * (optiCombine(s(key), r(key)))
    }
  }
  
  private def optiCombine(n : Int, k : Int) : Double = {
    if (n >= k) 
      (optiFactorial(n,n-k) / factorial(k)).toDouble
    else 
      0
  }
  
  private def optiFactorial(n : Int, k : Int, acc : BigInt = 1) : BigInt = {
   if(n == k) 
     acc
   else
     optiFactorial(n-1,k,acc*n)
  }
  
  private def combine(n : Int, k : Int) : Int = {
    if (n >= k) 
      (factorial(n) / (factorial(k) * factorial(n-k))).toInt
    else 
        0
  }

  private def factorial(i : Int, acc : BigInt = 1) : BigInt = {
    if (i <= 1)
      acc
    else
      factorial(i-1,acc*i)
  }
  
}