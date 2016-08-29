
val lv = Vector(Vector('S', 'T'), Vector('o', 'o'), Vector('o', 'o'))

lv(0)(0)

val level =
  """ooo-------
    |oSoooo----
    |ooooooooo-
    |-ooooooooo
    |-----ooToo
    |------ooo-""".stripMargin


private lazy val vector: Vector[Vector[Char]] =
  Vector(level.split("\n").map(str => Vector(str: _*)): _*)

vector(4)(7)


val ls = List(1,2,3,6)

