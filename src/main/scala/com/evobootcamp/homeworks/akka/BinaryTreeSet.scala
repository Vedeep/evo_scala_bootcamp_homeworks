package com.evobootcamp.homeworks.akka

import akka.actor._
import com.evobootcamp.homeworks.akka.BinaryTreeSet.Operation.CleanupRemoved

import scala.concurrent.duration._

object BinaryTreeSet {

  sealed trait Operation {
    def requester: ActorRef
    def id: Int
    def elem: Int
  }

  // requests with identifier `id`
  // `requester` should be notified when an operation is completed.
  object Operation {
    // insert an element `elem` into the tree.
    final case class Insert(requester: ActorRef, id: Int, elem: Int) extends Operation

    // check whether an element `elem` is present in the tree
    final case class Contains(requester: ActorRef, id: Int, elem: Int) extends Operation

    // remove the element `elem` from the tree
    final case class Remove(requester: ActorRef, id: Int, elem: Int) extends Operation

    // garbage collector
    final case class CleanupRemoved(requester: ActorRef)
    final case class StopChild(ref: ActorRef)
  }

  sealed trait OperationReply {
    def id: Int
  }

  object OperationReply {
    // answer to the Contains request with identifier `id`.
    // `result` is true if and only if the element is present in the tree
    final case class ContainsResult(id: Int, result: Boolean) extends OperationReply

    // successful completion of an insert or remove operation
    final case class OperationFinished(id: Int) extends OperationReply

    // successful cleaned
    final case class CleanupFinished(id: Int, ref: ActorRef, removed: Boolean) extends OperationReply
  }

  object RunGC
}

final class BinaryTreeSet extends Actor {
  import BinaryTreeSet._
  import context.dispatcher

  private val root = createRoot

  override def receive: Receive = {
    case m: Operation => root ! m
    case RunGC => root ! CleanupRemoved(self)
  }

  private def createRoot: ActorRef = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true))

  override def postStop(): Unit = gc.cancel()

  private val gc = context
    .system
    .getScheduler
    .scheduleAtFixedRate(5.seconds, 5.seconds, self, RunGC)
}
