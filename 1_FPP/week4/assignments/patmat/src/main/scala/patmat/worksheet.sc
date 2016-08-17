def pack(xs: List[Char]): List[List[Char]] = xs match {
  case Nil => Nil
  case x :: xs1 => (xs takeWhile(c => c == x)) :: pack(xs dropWhile(c => c == x))
}

def times(chars: List[Char]): List[(Char, Int)] = {
  val sortedChars = chars.sorted
  pack(sortedChars).map(l => (l.head, l.length))
}

var l2 = times(List('a', 'a', 'a', 'b', 'b', 'c', 'a', 'a'))



