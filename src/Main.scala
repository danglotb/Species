
/**
 * @author danglot
 */
object Main extends App {
  
  def run(t : Double, tmax : Double, s : System, out : String) : String = {
    if (t > tmax)// || s.cantApplyAnyReaction)
      out
    else {
      val To = (1/s.sumHA)*Math.log(1/(new java.util.Random().nextDouble))
      val indexReaction = s.chooseReaction
      val state = s.applyReaction(indexReaction)
      run(t+To, tmax, state, out+((t+To)+"\t"+s.values))
    }
  }
  
  val reactions = List(new Reaction(Map(("A"-> 1), ("B"-> 1)), Map(("C"-> 1)), 1.0),
    new Reaction(Map(("A"-> 1), ("C"-> 1)), Map(("D"-> 1)), 0.5))

  val initState = Map("A" -> 150, "B" -> 50, "C" -> 10, "D" -> 0)

  val s = new System(reactions, initState)

  val tmax : Double = 0.4

  val out = "To\t#A\t#B\t#C\t#D\n"
  
  Some(new java.io.PrintWriter("out.csv")).foreach { p => p.write(run(0.0, tmax, s, out)); p.close}
  
}