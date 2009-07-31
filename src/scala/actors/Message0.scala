/*
 * Message0.scala
 *
 * (c) Arthur Peters
 * Licensed under the LGPL v2 or later.
 *
 * If you want a different license talk to me.
 */

package scala.actors
import scala.reflect.Manifest

class Message0(owner : DeclActor) extends Message(owner) {

    def unapply(v:Any) : Boolean = v match {
        case ActorMessage(src, ()) if src eq this => true
        case _ => false
    }
}

class SyncMessage0[R](implicit owner : DeclActor, rClass : Manifest[R]) extends Message0(owner) with SyncMessage[R] {
    def invoke() : R = castReturn(owner !? ActorMessage(this, ()))
    def apply() = invoke()
    def !?() = invoke()

    def invokeFuture() : Future[R] = owner !! (ActorMessage(this, ()), castReturnPartialFunc)
    def !!() = invokeFuture()
}
class AsyncMessage0(implicit owner : DeclActor) extends Message0(owner) {
    def send() : Unit = owner ! ActorMessage(this, ())
    def !() = send()
}
