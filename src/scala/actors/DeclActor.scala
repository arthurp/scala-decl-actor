/*
 * DeclActor.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package scala.actors

import Actor._
import scala.reflect.Manifest

private[actors] case class ActorMessage(msg:Message, args:Any)

class Message(protected val owner : DeclActor) {
    lazy val name : Option[String] = {
        val methods = owner.getClass.getMethods
        //println(methods.deepToString())
        val typeMatches = methods.filter( f => (f.getReturnType eq this.getClass) && (f.getParameterTypes().size == 0) && (f.getModifiers == java.lang.reflect.Modifier.PUBLIC) )
        //println(typeMatches.deepToString())
        val valMatches = typeMatches.filter( f => f.invoke(owner) eq this )
        //println(valMatches.deepToString())
        valMatches.firstOption.map(_.getName)
    }

    override def toString() = (name map (n => getClass.getSimpleName + "<"+n+">")) getOrElse super.toString()
}

trait SyncMessage[R] {
    this : Message =>
    
    private final def checkType(x:Any)(implicit rClass : Manifest[R]) = rClass.erasure.isInstance(x)
    private final def throwError(x:Any)(implicit rClass : Manifest[R]) = throw new Error("Incorrect return type from " + this + " should be " + rClass + " was " + (x.asInstanceOf[AnyRef]).getClass + ".")

    protected final def castReturnPartialFunc(implicit rClass : Manifest[R]) = new PartialFunction[Any, R] {
        override def isDefinedAt(x: Any): Boolean = checkType(x)
        override def apply(x:Any): R = castReturn(x)
    }

    protected final def castReturn(x:Any)(implicit rClass : Manifest[R]) = if (checkType(x)) {
        x.asInstanceOf[R]
    } else {
        throwError(x)
    }

    def reply(r:R) = {
        owner.reply(r)
    }
}

abstract class DeclActor extends Actor {
    implicit protected val ownerOfMessages = this;
}
