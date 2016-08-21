type Occurrences = List[(Char, Int)]

def combinations(occurrences: Occurrences): List[Occurrences] = {
  def combinationsAcc(subOccurrences: Occurrences): List[Occurrences] = {
    if (subOccurrences.isEmpty) List(List())
    else
      for {
        acc <- combinationsAcc(subOccurrences.tail)
        times <- 0 to subOccurrences.head._2
      } yield (subOccurrences.head._1, times) :: acc
  }
  combinationsAcc(occurrences) map (occ => occ filter (p => p._2 != 0))
}

val myOccur = List(('a',2), ('b',2))

val combis = combinations(myOccur)

val myMap = myOccur.toMap

def subtract(x: Occurrences, y: Occurrences): Occurrences = {
  def subTerm(occ: Map[Char, Int], tup: (Char, Int)): Map[Char, Int] = {
    occ + (tup._1 -> (occ(tup._1) - tup._2))
  }
  y.foldLeft(x)((occ: Occurrences, tup: (Char, Int)) => subTerm(occ.toMap, tup).toList.filter(tup => tup._2 != 0))
}

subtract(myOccur, List())