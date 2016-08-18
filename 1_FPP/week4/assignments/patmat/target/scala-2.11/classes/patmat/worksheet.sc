import patmat.Huffman._

def pack(xs: List[Char]): List[List[Char]] = xs match {
  case Nil => Nil
  case x :: xs1 => (xs takeWhile(c => c == x)) :: pack(xs dropWhile(c => c == x))
}

def times(chars: List[Char]): List[(Char, Int)] = {
  val sortedChars = chars.sorted
  pack(sortedChars).map(l => (l.head, l.length))
}

var l2 = times(List('a', 'a', 'a', 'b', 'b', 'c', 'a', 'a'))



val l1 = Nil


def freqsToLeafs(freqs: List[(Char, Int)]): List[Leaf] = freqs match {
  case Nil => Nil
  case f :: fq1 => Leaf(f._1, f._2) :: freqsToLeafs(fq1)
}

def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] =
  freqsToLeafs(freqs).sortBy(_.weight) //ascending order by weight

val mylist = List(('a',1), ('b', 5), ('c', 2), ('d',0))

val mylist_sorted = makeOrderedLeafList(mylist)


val secret = decodedSecret
