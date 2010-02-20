/*
 * Message8.scala
 *
 * (c) 2010 Arthur Peters
 * Licensed under the LGPL v2 or later.
 *
 * If you want a different license talk to me.
 */

package scala.actors
import scala.reflect.Manifest

class Message8[A1, A2, A3, A4, A5, A6, A7, A8](owner : DeclActor) extends Message(owner) {

    def unapply(v:Any) : Option[(A1, A2, A3, A4, A5, A6, A7, A8)] = v match {
        case ActorMessage(src, (a1, a2, a3, a4, a5, a6, a7, a8)) if src eq this => Some((a1.asInstanceOf[A1], a2.asInstanceOf[A2], a3.asInstanceOf[A3], a4.asInstanceOf[A4], a5.asInstanceOf[A5], a6.asInstanceOf[A6], a7.asInstanceOf[A7], a8.asInstanceOf[A8]))
        case _ => None
    }
}

class SyncMessage8[R, A1, A2, A3, A4, A5, A6, A7, A8](implicit owner : DeclActor, protected[this] val rClass : Manifest[R]) extends Message8[A1, A2, A3, A4, A5, A6, A7, A8](owner) with SyncMessage[R] {
    def invoke(a1:A1, a2:A2, a3:A3, a4:A4, a5:A5, a6:A6, a7:A7, a8:A8) : R = castReturn(owner !? ActorMessage(this, (a1, a2, a3, a4, a5, a6, a7, a8)))
    def apply(a1:A1, a2:A2, a3:A3, a4:A4, a5:A5, a6:A6, a7:A7, a8:A8) = invoke(a1, a2, a3, a4, a5, a6, a7, a8)
    def !?(a1:A1, a2:A2, a3:A3, a4:A4, a5:A5, a6:A6, a7:A7, a8:A8) = invoke(a1, a2, a3, a4, a5, a6, a7, a8)

    def invokeFuture(a1:A1, a2:A2, a3:A3, a4:A4, a5:A5, a6:A6, a7:A7, a8:A8) : Future[R] = owner !! (ActorMessage(this, (a1, a2, a3, a4, a5, a6, a7, a8)), castReturnPartialFunc)
    def !!(a1:A1, a2:A2, a3:A3, a4:A4, a5:A5, a6:A6, a7:A7, a8:A8) = invokeFuture(a1, a2, a3, a4, a5, a6, a7, a8)
}
class AsyncMessage8[A1, A2, A3, A4, A5, A6, A7, A8](implicit owner : DeclActor) extends Message8[A1, A2, A3, A4, A5, A6, A7, A8](owner) {
    def send(a1:A1, a2:A2, a3:A3, a4:A4, a5:A5, a6:A6, a7:A7, a8:A8) : Unit = owner ! ActorMessage(this, (a1, a2, a3, a4, a5, a6, a7, a8))
    def !(a1:A1, a2:A2, a3:A3, a4:A4, a5:A5, a6:A6, a7:A7, a8:A8) = send(a1, a2, a3, a4, a5, a6, a7, a8)
}
