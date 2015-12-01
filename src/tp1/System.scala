
package tp1

/**
 * @author danglot
 */
class System(r: List[Reaction], s: Map[String, Int]) {

  def state = s

  def reactions = r

  def cantApplyAnyReaction : Boolean = {
    r.foreach { reaction =>
      if (reaction.reactants.keys.foldLeft(true) {case (acc, key) => acc && (s(key) >= reaction.reactants(key))})
        return true
    }
    false
  }

  def chooseReaction: Int = choose(getH, new java.util.Random().nextDouble, 0.0, 0, sumHA)

  private def choose(h: List[Double], rand: Double, acc: Double, index: Int, totalHA: Double): Int = {
    if (acc > rand || index == h.length)
      index - 1
    else
      choose(h, rand, acc + ((h(index) * r(index).speed) / totalHA), index + 1, totalHA)
  }

  def values: String = {
    s.keys.fold("") {
      case (acc, key) => acc + s(key) + "\t"
    } + "\n"
  }

  def applyReaction(index: Int): System = {
    val m = update(s, r(index).reactants, index, { _ - _ })
    new System(r, update(m, r(index).products, index, { _ + _ }))
  }

  def sumHA: Double = {
    val list = listH(Nil, 0)
    list.foldLeft(0.0) {
      case (acc, h) => acc + (h * r(list.indexOf(h)).speed)
    }
  }

  def getH: List[Double] = listH(Nil, 0)

  private def listH(l: List[Double], index: Int): List[Double] = {
    if (index == r.length)
      l
    else
      listH(l :+ r(index).computeH(s), index + 1)
  }

  private def update(m: Map[String, Int], mapApply: Map[String, Int], index: Int, op: ((Int, Int) => Int)): Map[String, Int] = {
    mapApply.keys.foldLeft(m) {
      case (acc, key) =>
        if (acc.contains(key))
          acc.updated(key, op(m(key), (mapApply(key))))
        else
          acc + (key -> op(m(key), (mapApply(key))))
    }
  }

}