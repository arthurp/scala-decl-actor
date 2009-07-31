/*
 * Message3.scala
 *
 * (c) Arthur Peters
 * Licensed under the LGPL v2 or later.
 *
 * If you want a different license talk to me.
 */

package scala.actors
import scala.reflect.Manifest

class Message3[A1, A2, A3](owner : DeclActor) extends Message(owner) {

    def unapply(v:Any) : Option[(A1, A2, A3)] = v match {
        case ActorMessage(src, (a1:A1, a2:A2, a3:A3)) if src eq this => Some((a1, a2, a3))
        case _ => None
    }
}

class SyncMessage3[R, A1, A2, A3](implicit owner : DeclActor, rClass : Manifest[R]) extends Message3[A1, A2, A3](owner) with SyncMessage[R] {
    def invoke(a1:A1, a2:A2, a3:A3) : R = castReturn(owner !? ActorMessage(this, (a1, a2, a3)))
    def apply(a1:A1, a2:A2, a3:A3) = invoke(a1, a2, a3)
    def !?(a1:A1, a2:A2, a3:A3) = invoke(a1, a2, a3)

    def invokeFuture(a1:A1, a2:A2, a3:A3) : Future[R] = owner !! (ActorMessage(this, (a1, a2, a3)), castReturnPartialFunc)
    def !!(a1:A1, a2:A2, a3:A3) = invokeFuture(a1, a2, a3)
}
class AsyncMessage3[A1, A2, A3](implicit owner : DeclActor) extends Message3[A1, A2, A3](owner) {
    def send(a1:A1, a2:A2, a3:A3) : Unit = owner ! ActorMessage(this, (a1, a2, a3))
    def !(a1:A1, a2:A2, a3:A3) = send(a1, a2, a3)
}
