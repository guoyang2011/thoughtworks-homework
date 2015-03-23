package cn.homework.thoughtworks

/**
 * Created by yangguo on 15-3-16.
 */
object ConferenceTrackManagementSolution {
  /**
   * Problem Two: Conference Track Management
   * You are planning a big programming conference and have received many proposals which have passed the initial screen process but you're having trouble fitting them into the time constraints of the day -- there are so many possibilities! So you write a program to do it for you.
	 * •	The conference has multiple tracks each of which has a morning and afternoon session.
	 * •	Each session contains multiple talks.
	 * •	Morning sessions begin at 9am and must finish before 12 noon, for lunch.
	 * •	Afternoon sessions begin at 1pm and must finish in time for the networking event.
	 * •	The networking event can start no earlier than 4:00 and no later than 5:00.
	 * •	No talk title has numbers in it.
	 * •	All talk lengths are either in minutes (not hours) or lightning (5 minutes).
	 * •	Presenters will be very punctual; there needs to be no gap between sessions.
   * Note that depending on how you choose to complete this problem, your solution may give a different ordering or combination of talks into tracks. This is acceptable; you don’t need to exactly duplicate the sample output given here.
   *
   * Test input:
   * Writing Fast Tests Against Enterprise Rails 60min
   * Overdoing it in Python 45min
   * Lua for the Masses 30min
   * Ruby Errors from Mismatched Gem Versions 45min
   * Common Ruby Errors 45min
   * Rails for Python Developers lightning
   * Communicating Over Distance 60min
   * Accounting-Driven Development 45min
   * Woah 30min
   * Sit Down and Write 30min
   * Pair Programming vs Noise 45min
   * Rails Magic 60min
   * Ruby on Rails: Why We Should Move On 60min
   * Clojure Ate Scala (on my project) 45min
   * Programming in the Boondocks of Seattle 30min
   * Ruby vs. Clojure for Back-End Development 30min
   * Ruby on Rails Legacy App Maintenance 60min
   * A World Without HackerNews 30min
   * User Interface CSS in Rails Apps 30min
   *
   * Test output: 
   * Track 1:
   * 09:00AM Writing Fast Tests Against Enterprise Rails 60min
   * 10:00AM Overdoing it in Python 45min
   * 10:45AM Lua for the Masses 30min
   * 11:15AM Ruby Errors from Mismatched Gem Versions 45min
   * 12:00PM Lunch
   * 01:00PM Ruby on Rails: Why We Should Move On 60min
   * 02:00PM Common Ruby Errors 45min
   * 02:45PM Pair Programming vs Noise 45min
   * 03:30PM Programming in the Boondocks of Seattle 30min
   * 04:00PM Ruby vs. Clojure for Back-End Development 30min
   * 04:30PM User Interface CSS in Rails Apps 30min
   * 05:00PM Networking Event
   *
   * Track 2:
   * 09:00AM Communicating Over Distance 60min
   * 10:00AM Rails Magic 60min
   * 11:00AM Woah 30min
   * 11:30AM Sit Down and Write 30min
   * 12:00PM Lunch
   * 01:00PM Accounting-Driven Development 45min
   * 01:45PM Clojure Ate Scala (on my project) 45min
   * 02:30PM A World Without HackerNews 30min
   * 03:00PM Ruby on Rails Legacy App Maintenance 60min
   * 04:00PM Rails for Python Developers lightning
   * 05:00PM Networking Event
   */
  import Util._

  /**
   *
   * @param tasks 所有需要处理的事件
   * @tparam K 事件类型
   */
  class SolutionTwo[K](val tasks:Task[K]) {

    /**
     *
     * @param amTime (上午可用于安排任务总的时间,上午安排可调节时间)
     * @param pmTime (下午可用于安排任务最大时间,根据下午可调节时间)
     * @return
     */
    def scheduleDailyTask(amTime: (Int,Int) = (3 * 60,0), pmTime: (Int, Int) = (4 * 60, 60)) = {
      require(amTime._1>=0&&amTime._2>=0&&pmTime._1>=0&&pmTime._2>=0,"Invalid Argument,All amTime Items And All pmTime Items Must >=0")
      var dailySchedule: DailyTask[K] = List()
      var tempTasks: List[Task[K]] = List()
      var cFactor=amTime._2//上午的收敛因子
      /**
       * 问题分析，此问题为双排列组合问题，在不考虑排序问题的情况下，先对上午的进行排序,满足要求的组合为am=C(19,x1),满足下午要求的排列组合为pm=C(19-x1,x2),最终满足要求的排列组合为am*pm
       * @param remainedTime 迭代计算中剩余时间
       * @param remainTasks 迭代计算中剩余为安排的任务
       * @param scheduleTasked 迭代中已经安排的任务数
       */
      def scheduleTask(remainedTime: Int, remainTasks: Task[K], scheduleTasked: Task[K]): Unit = {
        for (r <- remainTasks) {
          if (remainedTime >= 0) {
            val currentRemainedTime = remainedTime - r._2
            val currentRemainTasks = remainTasks.splitAt(remainTasks.indexOf(r) + 1)._2.filter(_._2 <= currentRemainedTime)
            if (remainedTime >= 0 && remainedTime <= cFactor) {
              tempTasks = scheduleTasked :: tempTasks
              if (currentRemainedTime > 0) scheduleTask(currentRemainedTime, currentRemainTasks, r :: scheduleTasked)
            }
            if (currentRemainedTime > 0) {
              for (task <- currentRemainTasks) {
                val nextRemainTasks = currentRemainTasks.splitAt(currentRemainTasks.indexOf(task) + 1)._2
                scheduleTask(currentRemainedTime - task._2, nextRemainTasks, task :: r :: scheduleTasked)
              }
            }
          }
        }
      }
      scheduleTask(amTime._1, tasks, List())
      val amTasks = tempTasks
      cFactor=pmTime._2//下午的收敛因子
      for {amTask <- amTasks} {
        val remainPMTasks = tasks.filterNot {
          case t => amTask.exists(_ == t)
        }.toList
        tempTasks = List()
        scheduleTask(pmTime._1, remainPMTasks, List())
        dailySchedule = (amTask -> tempTasks) :: dailySchedule
      }
      dailySchedule
    }
  }
  object SolutionTwo {
    def apply(str:String) = {
      val input = str
      val sample = input.split("\n").map { t =>
        val line = t.trim
        val splitIndex = line.lastIndexOf(" ")
        val minute = line.substring(splitIndex + 1) match {
          case "lightning" => 5
          case min => min.substring(0, min.indexOf('m')).toInt
        }
        (line -> minute)
      }
      new SolutionTwo[String](sample.toList)
    }
    def resultToTerminal[K](item:(Task[K],Task[K]),launchEvent:(K,Int),networkEvent:(K,Int),index:Int,startTime:Int=9*60): Unit = {
      val dailySchedule = item._1 ::: launchEvent :: item._2:::List(networkEvent)
      var time = 9*60
      val pmStart=12*60
      println("Trace"+index)
      dailySchedule.map { item =>
        val key=(time/60).formatted("%02d")+":"+(time%60).formatted("%02d") +{
          if(time<pmStart) "AM"
          else "PM"
        }
        time += item._2
        key +" "+ item._1
      }.foreach(println)
    }
    def getSize[K](res:DailyTask[K])= {
      res.foldLeft(0L) { case (pre, next) =>
        pre + (next._1.size * next._2.size)
      }
    }
  }
  def main(args: Array[String]): Unit = {
    val sample = "Writing Fast Tests Against Enterprise Rails 60min\nOverdoing it in Python 45min\nLua for the Masses 30min\nRuby Errors from Mismatched Gem Versions 45min\nCommon Ruby Errors 45min\nRails for Python Developers lightning\nCommunicating Over Distance 60min\nAccounting-Driven Development 45min\nWoah 30min\nSit Down and Write 30min\nPair Programming vs Noise 45min\nRails Magic 60min\nRuby on Rails: Why We Should Move On 60min\nClojure Ate Scala (on my project) 45min\nProgramming in the Boondocks of Seattle 30min\nRuby vs. Clojure for Back-End Development 30min\nRuby on Rails Legacy App Maintenance 60min\nA World Without HackerNews 30min\nUser Interface CSS in Rails Apps 30min"
    val solverHandler=SolutionTwo(sample)
    val res=solverHandler.scheduleDailyTask()//返回所有满足要求的排列组合(不考虑任务执行的顺序,总共满足要求的排列组合有30711116种
    val traces = res.last._2.slice(0,if(res.last._2.size>10) 10 else res.last._2.size).map((res.last._1->_))//选择满足要求的部分结果进行数据转化
    var index=1
    for{
      trace<-traces//打印10条结果
    } {
      SolutionTwo.resultToTerminal[String](trace, ("Lunch" -> 60), ("Networking Event" -> 1),index)
      index+=1
    }
  }
}
