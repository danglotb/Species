

/**
 * @author danglot
 */
class Reaction(reactants: List[(String, Int)], product: List[(String, Int)], speed: Double) {

  def reactants() : List[(String, Int)] = reactants
  
  def speed(): Double = speed

  def apply(current : State) : State = {
    val newState = computeReactants(current,0,Map())
    new State(computeProduct(current, newState,0))
  }
  
  private def computeReactants(state : State, index : Int, map : Map[String, Int]) : Map[String, Int] = {
    if (index == reactants.length)
      map
    else
      computeReactants(state, index+1, map + (reactants(index)._1 -> (state.components().getOrElse(reactants(index)._1, 0) - reactants(index)._2)))
  }

  private def computeProduct(state : State ,s : Map[String, Int], index : Int) : Map[String, Int] = {
    if (index == product.length)
      s
    else
      computeProduct(state , s + (product(index)._1 -> (state.components().getOrElse(product(index)._1, 0) + product(index)._2)), index+1)
  }
  
  def getH(s : State) : Int = getInnerH(s,0,1)
  
  private def getInnerH(s : State, index: Int, h: Int) : Int = {
     if (index == reactants.length)
      h
    else {
      val newH = h * ((s.components()).getOrElse(reactants(index)._1, 0) / reactants(index)._2).toInt
      getInnerH(s, index + 1, newH)
    }
  }

}