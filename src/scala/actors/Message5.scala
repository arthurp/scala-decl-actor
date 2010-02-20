/*
 * Message5.scala
 *
 * (c) 2010 Arthur Peters
 * Licensed under the LGPL v2 or later.
 *
 * If you want a different license talk to me.
 */

package scala.actors
import scala.reflect.Manifest

class Message5[A1, A2, A3, A4, A5](owner : DeclActor) extends Message(owner) {

    def unapply(v:Any) : Option[(A1, A2, A3, A4, A5)] = v match {
        case ActorMessage(src, (a1, a2, a3, a4, a5)) if src eq this => Some((a1.asInstanceOf[A1], a2.asInstanceOf[A2], a3.asInstanceOf[A3], a4.asInstanceOf[A4], a5.asInstanceOf[A5]))
        case _ => None
    }
}

class SyncMessage5[R, A1, A2, A3, A4, A5](implicit owner : DeclActor, protected[this] val rClass : Manifest[R]) extends Message5[A1, A2, A3, A4, A5](owner) with SyncMessage[R] {
    def invoke(a1:A1, a2:A2, a3:A3, a4:A4, a5:A5) : R = castReturn(owner !? ActorMessage(this, (a1, a2, a3, a4, a5)))
    def apply(a1:A1, a2:A2, a3:A3, a4:A4, a5:A5) = invoke(a1, a2, a3, a4, a5)
    def !?(a1:A1, a2:A2, a3:A3, a4:A4, a5:A5) = invoke(a1, a2, a3, a4, a5)

    def invokeFuture(a1:A1, a2:A2, a3:A3, a4:A4, a5:A5) : Future[R] = owner !! (ActorMessage(this, (a1, a2, a3, a4, a5)), castReturnPartialFunc)
    def !!(a1:A1, a2:A2, a3:A3, a4:A4, a5:A5) = invokeFuture(a1, a2, a3, a4, a5)
}
class AsyncMessage5[A1, A2, A3, A4, A5](implicit owner : DeclActor) extends Message5[A1, A2, A3, A4, A5](owner) {
    def send(a1:A1, a2:A2, a3:A3, a4:A4, a5:A5) : Unit = owner ! ActorMessage(this, (a1, a2, a3, a4, a5))
    def !(a1:A1, a2:A2, a3:A3, a4:A4, a5:A5) = send(a1, a2, a3, a4, a5)
}
