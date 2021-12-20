package module1
import scala.util.control.Breaks._
import module1.list._



object App {

  def main(args: Array[String]): Unit = {

    /*
    def doomyFunc(a: String) = {
      Thread.sleep(1000)
      println(a)
    }

    val doomyFuncWithLoggingTime: String => Unit = hof.logRunningTime(doomyFunc)
    */
    

    println("--------- Option printIfAny ---------")
    val o = opt.Option(5)
    o.printIfAny
    opt.Option.None.printIfAny

    println("--------- Option zip ---------")
    val o1 = opt.Option(1)
    val o2 = opt.Option(2)

    o1.zip(o2).printIfAny
    o1.zip(opt.Option.None).printIfAny
    opt.Option.None.zip(o2).printIfAny

    println("--------- Option filter ---------")
    o1.filter(_ > 0).printIfAny
    o2.filter(_ < 2).printIfAny
    val noi: opt.Option[Int] = opt.Option.None
    noi.filter(i => i*i >=0).printIfAny


    println("--------- List constructor ---------")
    val l = list.List(1,2,3,4,5)
    println(l)

    println("--------- mkString ---------")
    println(l.mkString(", "))

    println("--------- map ---------")
    println(l.map(_*2).mkString(", "))

    println("--------- filter ---------")
    println(l.filter(_ % 2 == 1).mkString(", "))

    println("--------- incList ---------")
    val il = ListUtils.incList(l)
    println(il.mkString(", "))

    println("--------- shoutString ---------")
    val cl = list.List("a", "b", "c", "d", "e")
    val scl = ListUtils.shoutString(cl)
    println(scl.mkString(", "))

      
  }


}








