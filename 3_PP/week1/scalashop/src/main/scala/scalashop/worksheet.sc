val width = 32
val from = 0
val end = 4
val r = Range(0, width, width / 3) :+ width

val slices = r zip r.tail


for (y <- from until end;
     x <- 0 until width) yield (x, y)