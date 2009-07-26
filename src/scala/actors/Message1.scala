/*
 * Message1.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package scala.actors
import scala.reflect.Manifest

class Message1[A1](owner : DeclActor) extends Message(owner) {
    def unapply(v:Any) : Option[A1] = v match {
        case ActorMessage(src, a1:A1) if src eq this => Some(a1)
        case _ => None
    }
}

class SyncMessage1[R, A1](implicit owner : DeclActor, rClass : Manifest[R]) extends Message1[A1](owner) with SyncMessage[R] {
    def invoke(a1:A1) : R = castReturn(owner !? ActorMessage(this, a1))
    def apply(a1:A1) = invoke(a1)
    def !?(a1:A1) = invoke(a1)

    def invokeFuture(a1:A1) : Future[R] = owner !! (ActorMessage(this, a1), castReturnPartialFunc)
    def !!(a1:A1) = invokeFuture(a1)
}

class AsyncMessage1[A1](implicit owner : DeclActor) extends Message1[A1](owner) {
    def send(a1:A1) : Unit = owner ! ActorMessage(this, a1)
    def !(a1:A1) = send(a1)
}
