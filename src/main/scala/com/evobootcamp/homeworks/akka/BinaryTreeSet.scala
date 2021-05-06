package com.evobootcamp.homeworks.akka

import akka.actor._

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
  }

  object GarbageCollector {
    case object RunGC

    final case class Cleanup(elems: List[Int])
    final case class CleanupFinished(elems: List[Int])
  }
}

final class BinaryTreeSet extends Actor with Stash {
  import BinaryTreeSet._
  import BinaryTreeSet.Operation._
  import BinaryTreeSet.OperationReply._
  import BinaryTreeSet.GarbageCollector._

  import context.dispatcher

  private val root = createRoot

  private def active: Receive = {
    case m: Operation => root ! m

    case _: OperationFinished => ()

    case RunGC =>
      context.become(stashing)
      root ! Cleanup(Nil)
  }

  private def stashing: Receive = {
    case _: Operation => stash()

    case _: OperationFinished => ()

    case CleanupFinished(elems) =>
      elems.foreach(root ! Insert(self, -1, _))
      context.become(active)
      unstashAll()
  }

  override def receive: Receive = active

  private def createRoot: ActorRef = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true))

  override def postStop(): Unit = gc.cancel()

  private val gc = context
    .system
    .getScheduler
    .scheduleAtFixedRate(5.seconds, 5.seconds, self, RunGC)
}
