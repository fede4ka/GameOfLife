object GoL extends App {

  val glider  = new Life(Vector((1,2),(2,3),(3,1),(3,2),(3,3)),10)
  val LWSS = new Life(Vector((3,2),(3,3),(3,4),(3,5),(4,1),(4,5),(5,5),(6,1),(6,4)),10)
  val diehard = new Life(Vector((2,8),(3,2),(3,3),(4,3),(4,7),(4,8),(4,9)),10)
  val rPent = new Life(Vector((2,3),(2,4),(3,2),(3,3),(4,3)),10)

  println("Hello there! Choose the new life pattern:" +
    "\nPress 1 to glider" +
    "\nPress 2 to LWSS"  +
    "\nPress 3 to diehard" +
    "\nPress 4 to R-pentomino"
  )

  var generation = scala.io.StdIn.readLine().toInt match {
    case 1 => glider
    case 2 => LWSS
    case 3 => diehard
    case 4 => rPent
    case _ => new Life(Vector(),10)
  }

  var loop: String = ""
  generation.printGrid()
  println("Press any key to continue, press q to stop")

  do {
    loop =  scala.io.StdIn.readLine().toString
    generation = generation.turn
    generation.printGrid()
    println("Press any to continue, press q to stop")
  } while (loop != "q")

  class Life(seed: Vector[(Int, Int)], size: Int) {
    private val grid =  Array.ofDim[Boolean](size, size)
    seed foreach (cell => containsLifeAt(cell._1, cell._2))

    def turn:GoL.Life = {
      val nextGeneration = new Life(Vector[(Int,Int)](), size)
      for (row <- 0 until size; col <- 0 until size)
        if (isLifeAt((row, col)) && nFrendsAround(row, col) == 2
          || nFrendsAround(row, col) == 3)
          nextGeneration.containsLifeAt(row, col)
      nextGeneration
    }

    private def nFrendsAround(row: Int, col: Int): Int =
      areaAround(row, col) count isLifeAt

    private def areaAround(row: Int, col: Int): Vector[(Int, Int)] = {
      Vector((row - 1, col - 1), (row, col - 1), (row + 1, col - 1), (row - 1, col),
        (row + 1, col), (row - 1, col + 1), (row, col + 1), (row + 1, col + 1)) map
        {case(x,y) => ((x + size) % size , (y + size) % size)}
    }

    def isLifeAt(cell: (Int, Int)): Boolean =
      grid(cell._1)(cell._2)

    def containsLifeAt(row: Int, col: Int):Unit =
      grid(row)(col) = true

    def printGrid():Unit = {
      println
      grid foreach {
        row => row foreach {
          cell => if (cell) print("O") else print(".")
        }
          println
      }
    }
  }
}