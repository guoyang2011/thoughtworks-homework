package cn.homework.thoughtworks

/**
 * Created by yangguo on 15-3-16.
 */
object TrainsSolution {
  /**
   *
   * Problem one: Trains
   * The local commuter railroad services a number of towns in Kiwiland.  Because of monetary concerns, all of the tracks are 'one-way.'  That is, a route from Kaitaia to Invercargill does not imply the existence of a route from Invercargill to Kaitaia.  In fact, even if both of these routes do happen to exist, they are distinct and are not necessarily the same distance!
   * The purpose of this problem is to help the railroad provide its customers with information about the routes.  In particular, you will compute the distance along a certain route, the number of different routes between two towns, and the shortest route between two towns.
   * Input:  A directed graph where a node represents a town and an edge represents a route between two towns.  The weighting of the edge represents the distance between the two towns.  A given route will never appear more than once, and for a given route, the starting and ending town will not be the same town.
   * Output: For test input 1 through 5, if no such route exists, output 'NO SUCH ROUTE'.  Otherwise, follow the route as given; do not make any extra stops!  For example, the first problem means to start at city A, then travel directly to city B (a distance of 5), then directly to city C (a distance of 4).
   *     1.	The distance of the route A-B-C.
   *     2.	The distance of the route A-D.
   *     3.	The distance of the route A-D-C.
   *     4.	The distance of the route A-E-B-C-D.
   *     5.	The distance of the route A-E-D.
   *     6.	The number of trips starting at C and ending at C with a maximum of 3 stops.  In the sample data below, there are two such trips: C-D-C (2 stops). and C-E-B-C (3 stops).
   *     7.	The number of trips starting at A and ending at C with exactly 4 stops.  In the sample data below, there are three such trips: A to C (via B,C,D); A to C (via D,C,D); and A to C (via D,E,B).
   *     8.	The length of the shortest route (in terms of distance to travel) from A to C.
   *     9.	The length of the shortest route (in terms of distance to travel) from B to B.
   *     10.	The number of different routes from C to C with a distance of less than 30.  In the sample data, the trips are: CDC, CEBC, CEBCDC, CDCEBC, CDEBC, CEBCEBC, CEBCEBCEBC.
   *
   *  Test Input:
   *   For the test input, the towns are named using the first few letters of the alphabet from A to D.  A route between two towns (A to B) with a distance of 5 is represented as AB5.
   *   Graph: AB5, BC4, CD8, DC8, DE6, AD5, CE2, EB3, AE7
   *   Expected Output:
   *   Output #1: 9
   *   Output #2: 5
   *   Output #3: 13
   *   Output #4: 22
   *   Output #5: NO SUCH ROUTE
   *   Output #6: 2
   *   Output #7: 3
   *   Output #8: 9
   *   Output #9: 9
   *   Output #10: 7
   */
  import Util._

  class SolutionOne[Key](lookup: Graph[Key]) {
    /**
     * one to five solution
     * 给定路径算距离
     * @param path
     * @param cost
     * @throws RuntimeException
     * @return
     */
    @throws[RuntimeException]
    def distance(path: List[Key], cost: Int = 0): Int = {
      path match {
        case from :: to :: next_path =>
          val dist = lookup(from).filter(_._2 == to)
          if (dist.size > 0) {
            val tempCost = dist.head._1 + cost
            if (next_path.isEmpty) tempCost
            else distance(to :: next_path, tempCost)
          } else throw new RuntimeException("NO SUCH ROUTE")
        case _ => throw new RuntimeException("Invalid Argument,Please Input path Length Must >=2")
      }
    }
    def existsRouter(router:Key)=lookup.exists(_._1==router)
    /**
     * 给定起始点和终点寻找合适路径
     * @param from 起始位置
     * @param to 目的地
     * @param maxIterations 最大迭代次数
     * @param maxDistance  最大距离
     * @param canCircle 是否准许出现环
     * @return
     */
    def searchRouters(from: Key, to: Key, maxIterations: Int = 12, maxDistance: Int = Int.MaxValue,canCircle:Boolean=true): List[(Int, List[(Int, Key)])] = {
      require(maxIterations > 0 && maxDistance > 0&&existsRouter(from)&&existsRouter(to), "Invalid Argument,maxIteration Or maxDistance Must >0 or Input Router Must In Routers")
      var list: List[(Int, List[(Int, Key)])] = List()
      /**
       * 支持多环查找
       * @param key
       * @param prePath
       * @param numIterations
       * @param distance
       */
      def doIterator(key: Key, prePath: List[(Int, Key)], numIterations: Int, distance: Int = 0): Unit = {
        if (numIterations <= maxIterations && distance <= maxDistance) {
          val next = lookup(key)
          val isFind=next.filter(_._2.equals(to)) match{
            case end::Nil=>{
              list=(numIterations,end::prePath)::list
              true
            }
            case _=> false
          }
          //如果没有找到或者可以出现环时继续迭代查找满足要求的路径
          if(!isFind||canCircle) next.foreach(n_next => doIterator(n_next._2, n_next :: prePath, numIterations + 1, distance + n_next._1))
        }
      }
      doIterator(from, List(), 1)
      list
    }
  }

  object SolutionOne {
    @throws[Exception]
    def apply(routers: String) = {
      var resource: collection.mutable.Map[String, List[(Int, String)]] = collection.mutable.Map()
      for (router <- routers.split(",").map(n => n.trim)) {
        val from = router.charAt(0).toString
        val to = router.charAt(1).toString
        val distance = router.substring(2).toInt
        resource.get(from) match {
          case Some(list) if !list.isEmpty => resource += (from -> ((distance -> to) :: list))
          case _ => resource += (from -> List((distance -> to)))
        }
      }//生成邻接表
      new SolutionOne[String](resource.toMap)
    }

    def resultToTerminal(solverHandler: SolutionOne[String]) = {
      List(("Output #1:", List("A", "B", "C")),
        ("Output #2:", List("A", "D")),
        ("Output #3:", List("A", "D", "C")),
        ("Output #4:", List("A", "E", "B", "C", "D")),
        ("Output #5:", (List("A", "E", "D")))).foreach {
        case (label, args) =>
          val res = try {
            solverHandler.distance(args)
          } catch {
            case ex: Throwable => ex.getMessage
          }
          println(label + res.toString)
      }
      println("Output #6:" + solverHandler.searchRouters("C", "C", 10,60,false).filter(_._1 <= 3).size)
      println("Output #7:" + solverHandler.searchRouters("A", "C", 10).filter(_._1 == 4).size)
      println("Output #8:" + solverHandler.searchRouters("A", "C", 10, 60).map(_._2.foldLeft(0)(_ + _._1)).sortBy(i => i).head)
      println("Output #9:" + solverHandler.searchRouters("B", "C", 10, 60).map(_._2.foldLeft(0)(_ + _._1)).sortBy(i => i).head)
      println("Output #10:" + solverHandler.searchRouters("C", "C", 10).map(_._2.foldLeft(0)(_ + _._1)).filter(_ < 30).size)
    }
  }

  def main(args: Array[String]) {
    type handler = List[(String, List[String], (List[String], Int) => Int)]
    val sample = "AB5, BC4, CD8, DC8, DE6, AD5, CE2, EB3, AE7"
    val solverHandler = SolutionOne(sample)//图的存储采用邻接表的方式存储
    SolutionOne.resultToTerminal(solverHandler)
  }

}
