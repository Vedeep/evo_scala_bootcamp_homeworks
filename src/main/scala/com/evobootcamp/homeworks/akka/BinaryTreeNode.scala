package com.evobootcamp.homeworks.akka

import akka.actor.{Actor, ActorRef, OneForOneStrategy, Props, SupervisorStrategy, Terminated}
import akka.stream.Supervision.Stop

object BinaryTreeNode {
  private sealed trait Position

  private case object Left extends Position
  private case object Right extends Position

  def props(elem: Int, initiallyRemoved: Boolean): Props = Props(new BinaryTreeNode(elem, initiallyRemoved))
}

final class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {
  import BinaryTreeNode._
  import BinaryTreeSet.Operation._
  import BinaryTreeSet.OperationReply._

  private val isRoot = initiallyRemoved

  private var subtrees = Map[Position, ActorRef]()
  private var removed = initiallyRemoved

  override def receive: Receive = {
    case msg @ Insert(_, _, _)   => doInsert(msg)
    case msg @ Contains(_, _, _) => doContains(msg)
    case msg @ Remove(_, _, _)   => doRemove(msg)

    case msg @ CleanupRemoved(_) => doCleanup(msg)
    case Terminated(ref) => doRemoveChild(ref)
    case StopChild(ref) =>
      doRemoveChild(ref)
      doStopChild(ref)
  }

  private def getPosition(e: Int): Option[Position] = {
    if (e > elem) Some(Right)
    else if (e < elem) Some(Left)
    else None
  }

  private def doInsert(m: Insert): Unit = {
    getPosition(m.elem) match {
      case Some(position) => subtrees.get(position) match {
        case Some(ref) => ref ! m
        case None => {
          val ref = context.actorOf(props(m.elem, false), "test-" + m.elem)
          context.watch(ref)
          subtrees = subtrees + (position -> ref)
          m.requester ! OperationFinished(m.id)
        }
      }
      case None => m.requester ! OperationFinished(m.id)
    }
  }

  private def doContains(m: Contains): Unit = {
    getPosition(m.elem) match {
      case Some(position) => subtrees.get(position) match {
        case Some(ref) => ref ! m
        case None => m.requester ! ContainsResult(m.id, false)
      }
      case None => m.requester ! ContainsResult(m.id, !removed && m.elem == elem)
    }
  }

  private def doRemove(m: Remove): Unit = {
    getPosition(m.elem) match {
      case Some(position) => subtrees.get(position) match {
        case Some(ref) => ref ! m
        case None => m.requester ! OperationFinished(m.id)
      }

      case None => {
        removed = true
        m.requester ! OperationFinished(m.id)
      }
    }
  }

  private def doCleanup(m: CleanupRemoved): Unit = {
    if (removed && !isRoot) {
      context.parent ! StopChild(self)
    } else {
      subtrees.values.foreach(_ ! m)
    }
  }

  private def doRemoveChild(ref: ActorRef): Unit = {
    subtrees = subtrees.filterNot({
      case (_, actRef) => actRef == ref
    })
  }

  private def doStopChild(ref: ActorRef): Unit = {
    context.stop(ref)
  }

  override def postStop(): Unit = {
    subtrees = Map.empty
  }


}
