/*
 * GenerateMessages.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package generators

object GenerateMessages {
    def genThings(i:Int)(f : (Int) => String) : String = (1 to i).map(f).mkString("", ", ", "")

    def genArgs(i:Int) : String = genThings(i)(n=> "a"+n)
    def genArgsDecl(i:Int) : String = genThings(i)(n=> "a"+n+":A"+n)
    def genTypes(i:Int) : String = genThings(i)(j=>"A" + j)
    def genTupleType(i:Int) : String = i match {
        case 1 => genTypes(i)
        case _ => "(" + genTypes(i) + ")"
    }
    def genTupleArgsDecl(i:Int) : String = i match {
        case 1 => genArgsDecl(i)
        case _ => "(" + genArgsDecl(i) + ")"
    }

    def genMessage(i:Int) :String = {
        """package scala.actors
import scala.reflect.Manifest

class Message%%i%%[%%types%%](owner : DeclActor) extends Message(owner) {
    def unapply(v:Any) : Option[%%tuple_type%%] = v match {
        case ActorMessage(src, %%tuple_args_decl%%) if src eq this => Some((%%args%%))
        case _ => None
    }
}

class SyncMessage%%i%%[R, %%types%%](implicit owner : DeclActor, rClass : Manifest[R]) extends Message%%i%%[%%types%%](owner) with SyncMessage[R] {
    def invoke(%%args_decl%%) : R = castReturn(owner !? ActorMessage(this, (%%args%%)))
    def apply(%%args_decl%%) = invoke(%%args%%)
    def !?(%%args_decl%%) = invoke(%%args%%)

    def invokeFuture(%%args_decl%%) : Future[R] = owner !! (ActorMessage(this, (%%args%%)), castReturnPartialFunc)
    def !!(%%args_decl%%) = invokeFuture(%%args%%)
}

class AsyncMessage%%i%%[%%types%%](implicit owner : DeclActor) extends Message%%i%%[%%types%%](owner) {
    def send(%%args_decl%%) : Unit = owner ! ActorMessage(this, (%%args%%))
    def !(%%args_decl%%) = send(%%args%%)
}
""".
        replaceAll("%%types%%", genTypes(i)).
        replaceAll("%%tuple_type%%", genTupleType(i)).
        replaceAll("%%args_decl%%", genArgsDecl(i)).
        replaceAll("%%args%%", genArgs(i)).
        replaceAll("%%i%%", ""+i).
        replaceAll("%%tuple_args_decl%%", genTupleArgsDecl(i))
    }

    def main(args : Array[String]) {
        for (i <- 1 to 8) {
            import java.io.{FileOutputStream, FileWriter}
            val fname = "src/scala/actors/Message" + i + ".scala"
            val f = new FileWriter(fname)
            f.write(genMessage(i))
            f.close
        }
        println("Done")
    }
}
