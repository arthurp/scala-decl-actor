/*
 * GenerateMessages.scala
 *
 * (c) 2010 Arthur Peters
 * Licensed under the LGPL v2 or later.
 *
 * If you want a different license talk to me.
 */

package generators

object GenerateMessages {
    class Message(val i:Int) {
        protected def genThings(f : (Int) => String) : String = if(i == 0) "" else (1 to i).map(f).mkString("", ", ", "")
        def argNames = genThings(n=> "a"+n)
        def argsWithCasts = genThings(n=> "a"+n+".asInstanceOf[A"+n+"]")
        def argsDecl = genThings(n=> "a"+n+":A"+n)
        def typeArgNames = genThings(j=>"A" + j)
        def tupleType = "(" + typeArgNames + ")"
        def tupleArgsDecl = "(" + argNames + ")"
        def fileName = "Message%d.scala".format(i)
        
        def unapplyMethod = {
<segment>
    def unapply(v:Any) : Option[{tupleType}] = v match {{
        case ActorMessage(src, {tupleArgsDecl}) if src eq this => Some(({argsWithCasts}))
        case _ => None
    }}</segment>.text
        }

        def asyncTypeArgs = "[" + typeArgNames + "]"
        def syncTypeArgs = "[R, " + typeArgNames + "]"

        def messageClass = {
<segment>class Message{i}{asyncTypeArgs}(owner : DeclActor) extends Message(owner) {{
{unapplyMethod}
}}</segment>.text
        }
        def syncMessageClass = {
<segment>class SyncMessage{i}{syncTypeArgs}(implicit owner : DeclActor, protected[this] val rClass : Manifest[R]) extends Message{i}{asyncTypeArgs}(owner) with SyncMessage[R] {{
    def invoke({argsDecl}) : R = castReturn(owner !? ActorMessage(this, ({argNames})))
    def apply({argsDecl}) = invoke({argNames})
    def !?({argsDecl}) = invoke({argNames})

    def invokeFuture({argsDecl}) : Future[R] = owner !! (ActorMessage(this, ({argNames})), castReturnPartialFunc)
    def !!({argsDecl}) = invokeFuture({argNames})
}}</segment>.text
        }
        def asyncMessageClass = {
<segment>class AsyncMessage{i}{asyncTypeArgs}(implicit owner : DeclActor) extends Message{i}{asyncTypeArgs}(owner) {{
    def send({argsDecl}) : Unit = owner ! ActorMessage(this, ({argNames}))
    def !({argsDecl}) = send({argNames})
}}</segment>.text
        }


        def apply() : String = {
<file name={fileName}>/*
 * {fileName}
 *
 * (c) 2010 Arthur Peters
 * Licensed under the LGPL v2 or later.
 *
 * If you want a different license talk to me.
 */

package scala.actors
import scala.reflect.Manifest

{messageClass}

{syncMessageClass}
{asyncMessageClass}
</file>.text
        }
    }

    class MessageZero extends Message(0) {
        override def unapplyMethod = {
<segment>
    def unapply(v:Any) : Boolean = v match {{
        case ActorMessage(src, {tupleArgsDecl}) if src eq this => true
        case _ => false
    }}</segment>.text
        }

        override def asyncTypeArgs = ""
        override def syncTypeArgs = "[R]"
    }

    class MessageOne extends Message(1) {
        override def tupleType = typeArgNames
        override def tupleArgsDecl = argNames
    }

    object Message {
        def apply(i:Int) = i match {
            case 0 => new MessageZero
            case 1 => new MessageOne
            case _ => new Message(i)
        }
    }

    def main(args : Array[String]) {
        for (i <- 0 to 8) {
            import java.io.{FileOutputStream, FileWriter}
            val m = Message(i)
            val fname = "src/scala/actors/" + m.fileName
            val f = new FileWriter(fname)
            f.write(m())
            f.close
        }
        println("Done")
    }
}
