/*
 * DeclActor.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package scala.actors

import Actor._
import scala.reflect.Manifest

case class ActorMessage(msg:Message, args:Any)

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
    protected def castReturnPartialFunc(implicit rClass : Manifest[R]) = new PartialFunction[Any, R] {
        override def isDefinedAt(x: Any): Boolean = rClass.erasure.isInstance(x)
        override def apply(x:Any): R = if (rClass.erasure.isInstance(x)) {
            x.asInstanceOf[R]
        } else {
            throw new Error("Incorrect return type from " + this + " should be " + rClass + " was " + (x.asInstanceOf[AnyRef]).getClass + ".")
        }
    }

    protected def castReturn(v:AnyRef)(implicit rClass : Manifest[R]) = v match {
        case r:R if rClass.erasure.isInstance(r) => r
        case _ => throw new Error("Incorrect return type from " + this + " should be " + rClass + " was " + v.getClass + ".")
    }
}

abstract class DeclActor extends Actor {
    implicit protected val ownerOfMessages = this;
}
