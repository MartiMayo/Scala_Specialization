import common._

2 / 3
val money = 2
money * 2 / 3



val l = List()


var x1: Int = 3
var x2: Int = 1
val z = x1 + x2


val chars = Array[Char]('(',  '(', ')', ')')

def parBalance(chars: Array[Char], threshold: Int): Boolean = {

  def traverse(idx: Int, until: Int): (Int, Int) = {
    var x1 = 0
    var x1_acc = 0
    for (i <- 0 until until) {
      if (chars(i) == '(') {
        if (x1_acc > 0) {
          x1 += x1_acc
          x1_acc = -1
        }
        else x1_acc -= 1

      }
      else if (chars(i) == ')') x1_acc += 1
    }
    if (x1_acc > 0) (x1 + x1_acc, 0)
    else (x1, -x1_acc)
  }

  def reduce(from: Int, until: Int): (Int, Int) = {
    if ((until - from) <= threshold) traverse(from, until)
    else {
      val m = (from + until) / 2
      val ((a1, a2), (b1, b2)) = parallel(reduce(from, m), reduce(m, until))
      if (a2 - b1 > 0) (a1, a2 - b1 + b2)
      else (a1 + b1 - a2, b2)
    }
  }

  reduce(0, chars.length) == (0, 0)
}

def balance(chars: Array[Char]): Boolean = {
  var counter = 0
  for (i <- 0 until chars.length) {
    print(i + ": " + counter.toString)
    if (chars(i) == '(') counter += 1
    else if(chars(i) == ')') counter -= 1
    if (counter < 0) return false
  }
  counter == 0
}

parBalance(chars, 1000)


balance(")(".toArray)
