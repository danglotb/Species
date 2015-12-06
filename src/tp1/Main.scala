
package tp1
/**
 *  @author danglot
 */
object Main extends App {
  
  def run(t : Double, tmax : Double, s : System, out : String) : String = {
    if (t > tmax || !s.cantApplyAnyReaction) {
      println(out)
      out
    } else {
      val To = (1/s.sumHA)*Math.log(1/(new java.util.Random().nextDouble))
      val indexReaction = s.chooseReaction
      val state = s.applyReaction(indexReaction)
      run(t+To, tmax, state, out+((t+To)+"\t"+s.values))
    }
  }
  
  val time = System.currentTimeMillis()
  
//  val reactions = List(new Reaction(Map(("A"-> 1), ("B"-> 1)), Map(("C"-> 1)), 1.0),
//    new Reaction(Map(("A"-> 1), ("C"-> 1)), Map(("D"-> 1)), 0.5))
//
//  val initState = Map("A" -> 150, "B" -> 50, "C" -> 10, "D" -> 0)
  
  val k = 0.1
  
  val reactions = List(new Reaction(Map(("A"-> 1)), Map(), k))
  
  val initValue = 0
  
  val initState = Map("A"->initValue.toInt)

  val s = new System(reactions, initState)

  val tmax : Double = 100

  val out = ""
  
  Some(new java.io.PrintWriter("out"+initValue)).foreach { p => p.write(run(0.0, tmax, s, out)); p.close}
  
  var str = "plot"
  
  var cpt = 2
  
  initState.keys.foreach { k =>
    str += " \'out"+initValue+"\' using 1:"+cpt+" title \'"+k+"\' with lines, "
    cpt += 1
  }
  
  Some(new java.io.PrintWriter("plot"+initValue+".plt")).foreach { p => p.write(str.substring(0,str.length-2)); p.close}
  
  println("Simulation end : "+ (System.currentTimeMillis() - time) + " ms")
  
}