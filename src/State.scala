
/**
 * @author danglot
 */
class State(components : Map[String, Int]) {
  
  def components() : Map[String, Int] = components
  
  def values() : String = {
    var ret : String = ""
    components.keys.foreach{ c => ret+=components.get(c).get+","}
    ret.substring(0,ret.length-1)
  }
  
  def cantApplyAny(reaction : List[Reaction]) : Boolean = {
    reaction.foreach { r => 
      var b = false
      println(components)
      r.reactants.foreach { react => 
        println(react._1)
        b |= react._2 >= components.get(react._1).get
      }
      if (b)
        return true
    }
    false
  }
  
}