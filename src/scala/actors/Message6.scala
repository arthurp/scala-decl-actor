/*
 * Message6.scala
 *
 * (c) Arthur Peters
 * Licensed under the LGPL v2 or later.
 *
 * If you want a different license talk to me.
 */

package scala.actors
import scala.reflect.Manifest

class Message6[A1, A2, A3, A4, A5, A6](owner : DeclActor) extends Message(owner) {

    def unapply(v:Any) : Option[(A1, A2, A3, A4, A5, A6)] = v match {
        case ActorMessage(src, (a1:A1, a2:A2, a3:A3, a4:A4, a5:A5, a6:A6)) if src eq this => Some((a1, a2, a3, a4, a5, a6))
        case _ => None
    }
}

class SyncMessage6[R, A1, A2, A3, A4, A5, A6](implicit owner : DeclActor, rClass : Manifest[R]) extends Message6[A1, A2, A3, A4, A5, A6](owner) with SyncMessage[R] {
    def invoke(a1:A1, a2:A2, a3:A3, a4:A4, a5:A5, a6:A6) : R = castReturn(owner !? ActorMessage(this, (a1, a2, a3, a4, a5, a6)))
    def apply(a1:A1, a2:A2, a3:A3, a4:A4, a5:A5, a6:A6) = invoke(a1, a2, a3, a4, a5, a6)
    def !?(a1:A1, a2:A2, a3:A3, a4:A4, a5:A5, a6:A6) = invoke(a1, a2, a3, a4, a5, a6)

    def invokeFuture(a1:A1, a2:A2, a3:A3, a4:A4, a5:A5, a6:A6) : Future[R] = owner !! (ActorMessage(this, (a1, a2, a3, a4, a5, a6)), castReturnPartialFunc)
    def !!(a1:A1, a2:A2, a3:A3, a4:A4, a5:A5, a6:A6) = invokeFuture(a1, a2, a3, a4, a5, a6)
}
class AsyncMessage6[A1, A2, A3, A4, A5, A6](implicit owner : DeclActor) extends Message6[A1, A2, A3, A4, A5, A6](owner) {
    def send(a1:A1, a2:A2, a3:A3, a4:A4, a5:A5, a6:A6) : Unit = owner ! ActorMessage(this, (a1, a2, a3, a4, a5, a6))
    def !(a1:A1, a2:A2, a3:A3, a4:A4, a5:A5, a6:A6) = send(a1, a2, a3, a4, a5, a6)
}
