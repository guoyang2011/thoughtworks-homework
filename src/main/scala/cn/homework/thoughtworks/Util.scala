package cn.homework.thoughtworks

/**
 * Created by yangguo on 15-3-16.
 */
object Util {
  type Graph[Key]=Map[Key, List[(Int, Key)]]
  type DailyTask[K]=List[(Task[K],List[Task[K]])]
  type Task[K]=List[(K,Int)]
}
