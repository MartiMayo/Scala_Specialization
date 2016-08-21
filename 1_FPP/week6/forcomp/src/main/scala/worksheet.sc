type Word = String

type Occurrences = List[(Char, Int)]

def wordOccurrences(w: Word) =
  (w.groupBy((c: Char) => c)
    map (t => t._1 -> t._2.length)
    ).toList.sortWith((a, b) => a._1 < b._1)


val myString = "martim"

val myMap = wordOccurrences(myString)


val example = List(0, 1, 2, 1, 0).groupBy((element: Int) => element)

example map (t => t._1 -> t._2.length)



myMap

myMap.toSet.subsets()
