/**
 * @author danglot
 */
object Main extends App {

  def run(t: Double, tmax: Double, currentState: State, s: System, out: String): String = {
    if (t > tmax || currentState.cantApplyAny(s reactions))
      out
    else {
      val hi = computeHi(currentState, s reactions, 0, Nil)
      val To = computeTo(s reactions, hi, 0, 0.0)
      val randIndexReaction = (new java.util.Random().nextDouble)
      val indexReaction = computeIndexReact(s reactions, hi, randIndexReaction, sumHiAi(s reactions, hi, 0, 0.0), 0.0, 0)
      val newState = ((s.reactions())(indexReaction)).apply(currentState)
      val newString = t+To+","+newState.values+"\n"
      run(t + To, tmax, newState, s, out + newString)
    }
  }

  private def sumHiAi(reactions: List[Reaction], hi: List[Int], index: Int, acc: Double): Double = {
    if (index == reactions.length)
      acc
    else
      sumHiAi(reactions, hi, index + 1, acc + (hi(index) * reactions(index).speed))
  }

  private def computeIndexReact(reactions: List[Reaction], hi: List[Int], rand: Double, max: Double, acc: Double, index: Int): Int = {
    if (acc >= rand || index == hi.length)
      index - 1
    else
      computeIndexReact(reactions, hi, rand, max, acc + ((hi(index) * reactions(index).speed) / max), index + 1)
  }

  private def computeHi(s: State, reactions: List[Reaction], index: Int, hi: List[Int]): List[Int] = {
    if (index == reactions.length)
      hi
    else
      computeHi(s, reactions, index + 1, hi :+ reactions(index).getH(s))
  }

  private def computeTo(reactions: List[Reaction], hi: List[Int], index: Int, To: Double): Double = {
    if (index == reactions.length)
      To * Math.log(1 / (new java.util.Random().nextDouble))
    else
      computeTo(reactions, hi, index + 1, To + ((reactions(index) speed) * hi(index)))
  }

  val reactions = List(new Reaction(List(("A", 1), ("B", 1)), List(("C", 1)), 1.0),
    new Reaction(List(("A", 1), ("C", 1)), List(("D", 1)), 0.5))

  val initState = new State(Map("A" -> 150, "B" -> 50, "C" -> 10, "D" -> 0))

  val s = new System(reactions, initState)

  val tmax : Double = 100000.0

  val out = "To,#A,#B,#C\n"

  Some(new java.io.PrintWriter("out.csv")).foreach { p => p.write(run(0.0, tmax, initState, s, out)); p.close}

}