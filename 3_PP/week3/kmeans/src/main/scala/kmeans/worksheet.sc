import scala.collection.GenSeq

val lst = List(8, 12, 14, 18, 24)

val myMap = lst.groupBy(x => x % 3)

for (i <- 0 until 3) {
  if (!lst.contains(i)) myMap += i -> List()
}