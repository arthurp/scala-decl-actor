/*
 * Message2.scala
 *
 * (c) Arthur Peters
 * Licensed under the LGPL v2 or later.
 *
 * If you want a different license talk to me.
 */

package scala.actors
import scala.reflect.Manifest

class Message2[A1, A2](owner : DeclActor) extends Message(owner) {

    def unapply(v:Any) : Option[(A1, A2)] = v match {
        case ActorMessage(src, (a1, a2)) if src eq this => Some((a1.asInstanceOf[A1], a2.asInstanceOf[A2]))
        case _ => None
    }
}

class SyncMessage2[R, A1, A2](implicit owner : DeclActor, protected[this] val rClass : Manifest[R]) extends Message2[A1, A2](owner) with SyncMessage[R] {
    def invoke(a1:A1, a2:A2) : R = castReturn(owner !? ActorMessage(this, (a1, a2)))
    def apply(a1:A1, a2:A2) = invoke(a1, a2)
    def !?(a1:A1, a2:A2) = invoke(a1, a2)

    def invokeFuture(a1:A1, a2:A2) : Future[R] = owner !! (ActorMessage(this, (a1, a2)), castReturnPartialFunc)
    def !!(a1:A1, a2:A2) = invokeFuture(a1, a2)
}
class AsyncMessage2[A1, A2](implicit owner : DeclActor) extends Message2[A1, A2](owner) {
    def send(a1:A1, a2:A2) : Unit = owner ! ActorMessage(this, (a1, a2))
    def !(a1:A1, a2:A2) = send(a1, a2)
}
