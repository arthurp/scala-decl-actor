package test

import scala.actors._
import scala.actors.Actor.loop

object Example1 {
  def main(args : Array[String]) : Unit = {
    class Actor1 extends DeclActor {
        val sync = new SyncMessage1[String, Int]
        val async = new AsyncMessage1[String]
        val whapp = new AsyncMessage0
        val done = new SyncMessage0[Unit]

		var state = ""

        override def act() = loop { react {
                case sync(i) => sync.reply( "(" + i + ")" )
                case async(s) => state = s
                case whapp() => println("Whapp: "+state)
                case done() => done.reply(()); exit
            } }
    }
    
    
    val a = new Actor1
    a.start
    
    val s1 : String = a.sync(42) 
    // Or
    val s2 : String = a.sync !?(43) 
    val fs : Future[String] = a.sync !!(84)
    a.async !("test")
    // Or
    a.async.send("test") 
    
    a.whapp !()
    
    a.done()
    
    println(s1)
    println(s2)
    println(fs())
    println(a.state)
  }
}
