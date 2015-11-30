/**
 * danglotb
 */
class Reaction(r : Map[String, Int], p : Map[String, Int], s : Double) {
  
  def reactants = r
  
  def products = p
  
  def speed = s
  
  def computeH(s : Map[String,Int]) : Double = {
    r.keys.foldLeft(1.0) {
      case (acc,key) => acc * (s(key)/r(key))
    }
  }
  
}