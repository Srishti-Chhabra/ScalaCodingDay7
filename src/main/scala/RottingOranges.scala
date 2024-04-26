import scala.collection.mutable.Queue

class RottingOranges {
  def orangesRotting(grid: Array[Array[Int]]): Int = {
    val n: Int = grid.size
    val m: Int = grid(0).size
    val queue = new Queue[(Int,Int)]
    val cnt: Int = (for{
      i <- (0 to n-1)
      j <- (0 to m-1)
    } yield{
      if(grid(i)(j) == 2)
        queue.enqueue((i,j))
      if(grid(i)(j) != 0)
        1
      else
        0
    }).sum

    def loop(count: Int, rottenOranges: Int): (Int,Int) = {
      if(queue.isEmpty)
        return (count, rottenOranges)
      val size: Int = queue.size
      val dr = List(-1, 0, +1, 0)
      val dl = List(0, +1, 0, -1)
      def innerLoop(i: Int): Int = {
        if(i >= size){
          return queue.size
        }
        val node = queue.dequeue()
        val row = node(0)
        val col = node(1)
        val adjNodes = for{
          i <- 0 to 3
        } yield (row+dr(i), col+dl(i))
        adjNodes.map{adjNode =>
          if(adjNode(0)>=0 && adjNode(0)<n && adjNode(1)>=0 && adjNode(1)<m){
            if(grid(adjNode(0))(adjNode(1)) == 1){
              queue.enqueue((adjNode(0), adjNode(1)))
              grid(adjNode(0))(adjNode(1)) = 2
            }
          }
        }
        innerLoop(i+1)
      }
      val res = innerLoop(0)
      if(res > 0)
        return loop(count+1, rottenOranges+res)
      return (count, rottenOranges)
    }
    val res = loop(0, queue.size)
    if(res(1) == cnt)
      res(0)
    else
      -1
  }
}
