/*
 * Checks.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package scala.actors

//import org.scalacheck.Prop
import org.scalatest.junit.JUnit3Suite
import org.scalatest.prop.Checkers

class Checks extends JUnit3Suite with Checkers {
    var lastS2 : String = _

    class Actor1 extends DeclActor {
        val sync = new SyncMessage1[String, Int]
        val sync2 = new SyncMessage2[String, Int, Int]
        val async = new AsyncMessage1[String]
        val async2 = new AsyncMessage2[String, String]
        val done = new SyncMessage0[Unit]
        val wrongType = new SyncMessage0[Int]

        var pre = ""

        override def act() = while(true) { receive {
                case sync(i) => reply( pre + "(" + i + ")" )//; println(pre + "(" + i + ")")
                case sync2(i, j) => reply( pre + "(" + i*j + ")" )//; println(pre + "(" + i*j + ")")
                case async(s) => pre = s
                case async2(s, s2) => pre = s; lastS2 = s2
                case done() => new RuntimeException().printStackTrace(); reply(()); exit
                case wrongType() => reply(pre)
            } }
    }

    def testSync1 {
        val a = new Actor1
        a.start

        check( (i:Int) => { a.sync(i) == "("+i+")" } )
        check( (i:Int) => { a.sync !?(i) == "("+i+")" } )

        check( (i:Int) => {
                val f = a.sync !!(i)
                f() == "("+i+")"
            } )
    }

    def testAsync1 {
        val a = new Actor1
        a.start
        
        check( (s:String, i:Int) => {
                a.async !(s)
                a.sync(i) == s + "("+i+")"
            } )
    }

    def testSync2 {
        val a = new Actor1
        a.start

        check( (i:Int, j:Int) => { a.sync2(i,j) == "("+i*j+")" } )
        check( (i:Int, j:Int) => { a.sync2 !?(i,j) == "("+i*j+")" } )

        check( (i:Int, j:Int) => {
                val f = a.sync2 !!(i,j)
                f() == "("+i*j+")"
            } )
    }
    def testAsync2 {
        val a = new Actor1
        a.start

        check( (s:String, s2:String, i:Int) => {
                a.async2 ! (s, s2)
                a.async !(s)
                a.sync(i) == s + "("+i+")" && lastS2 == s2
            } )
    }

    def testReturnUnit {
        val a = new Actor1
        a.start

        // This will throw if the bug is present
        a.done()
    }

    def testWrongType {
        val a = new Actor1
        a.start

        assert( 
            try {
                a.wrongType()
                false
            } catch {
                case _ : Error => true
            }
        )
    }
}