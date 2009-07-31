/*
 * Message4.scala
 *
 * (c) Arthur Peters
 * Licensed under the LGPL v2 or later.
 *
 * If you want a different license talk to me.
 */

package scala.actors
import scala.reflect.Manifest

class Message4[A1, A2, A3, A4](owner : DeclActor) extends Message(owner) {

    def unapply(v:Any) : Option[(A1, A2, A3, A4)] = v match {
        case ActorMessage(src, (a1:A1, a2:A2, a3:A3, a4:A4)) if src eq this => Some((a1, a2, a3, a4))
        case _ => None
    }
}

class SyncMessage4[R, A1, A2, A3, A4](implicit owner : DeclActor, rClass : Manifest[R]) extends Message4[A1, A2, A3, A4](owner) with SyncMessage[R] {
    def invoke(a1:A1, a2:A2, a3:A3, a4:A4) : R = castReturn(owner !? ActorMessage(this, (a1, a2, a3, a4)))
    def apply(a1:A1, a2:A2, a3:A3, a4:A4) = invoke(a1, a2, a3, a4)
    def !?(a1:A1, a2:A2, a3:A3, a4:A4) = invoke(a1, a2, a3, a4)

    def invokeFuture(a1:A1, a2:A2, a3:A3, a4:A4) : Future[R] = owner !! (ActorMessage(this, (a1, a2, a3, a4)), castReturnPartialFunc)
    def !!(a1:A1, a2:A2, a3:A3, a4:A4) = invokeFuture(a1, a2, a3, a4)
}
class AsyncMessage4[A1, A2, A3, A4](implicit owner : DeclActor) extends Message4[A1, A2, A3, A4](owner) {
    def send(a1:A1, a2:A2, a3:A3, a4:A4) : Unit = owner ! ActorMessage(this, (a1, a2, a3, a4))
    def !(a1:A1, a2:A2, a3:A3, a4:A4) = send(a1, a2, a3, a4)
}
