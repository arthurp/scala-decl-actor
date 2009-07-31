/*
 * DeclActor.scala
 *
 * (c) Arthur Peters
 * Licensed under the LGPL v2 or later.
 *
 * If you want a different license talk to me.
 */

package scala.actors

import Actor._
import scala.reflect.Manifest

/**
 The type used for passing messages using the Message... classes.
*/
private[actors] case class ActorMessage(msg:Message, args:Any)

/**
 The base class of all Message... classes.
*/
class Message(protected val owner : DeclActor) {
    /**
     The name of this message. This is computed using reflection so it may be
     slow to get it the first time.
    */
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

    // a hack is needed here because Unit is erased to void in some cases.
    private final def checkType(x:Any)(implicit rClass : Manifest[R]) = rClass.erasure.isInstance(x) || (rClass.erasure == Void.TYPE && x == ())
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

    /**
     Reply to this syncronous message. This simply calls owner.reply. However it
     will statically check that the reply is of the correct type.
    */
    def reply(r:R) = owner.reply(r)
}

/**
 All actors that use the Message... classes must extend DeclActor. It simply
 provides package private features needed by the Message... classes.
*/
abstract class DeclActor extends Actor {
    implicit protected val ownerOfMessages = this;
}
