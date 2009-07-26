/*
 * BasicTests.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package scala.actors

import org.junit.After
import org.junit.Before
import org.junit.Test
import org.junit.Assert._

class BasicTests {
    @Before
    def setUp: Unit = {
    }

    @After
    def tearDown: Unit = {
    }

    @Test
    def testBasics = {
    	class Actor1 extends DeclActor {
            val sync = new SyncMessage1[String, Int]
            val sync2 = new SyncMessage2[String, Int, Int]
            val async = new AsyncMessage1[String]
            val async2 = new AsyncMessage2[String, String]
            val done = new SyncMessage0[Unit]

            var pre = ""

            override def act() = while(true) { receive {
                    case sync(i) => reply( pre + "(" + i + ")" )
                    case sync2(i, j) => reply( pre + "(" + i*j + ")" )
                    case async(s) => pre = s
                    case async2(s, s2) => pre = s; println(s2)
                    case done() => new RuntimeException().printStackTrace(); reply(()); exit
		} }
	}

        val a = new Actor1
        a.start

        assertEquals("(2)", a.sync(2))
        assertEquals("(2)", a.sync !?(2))

        {
            val f = a.sync !!(3)
            assertEquals("(3)", f())
        }

        a.async !("pre")
        assertEquals("pre(12)", a.sync(12))
        
        {
            val f = a.sync !!(13)
            assertEquals("pre(13)", f())
        }

        println(a.sync2 !? (2, 10))
        println(a.async2 ! ("|", "There"))

        println(a.done())

    }

}
