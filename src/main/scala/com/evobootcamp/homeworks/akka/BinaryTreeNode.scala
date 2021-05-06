package com.evobootcamp.homeworks.akka

import akka.actor.{Actor, ActorRef, PoisonPill, Props, Terminated}
import com.evobootcamp.homeworks.akka.BinaryTreeSet.GarbageCollector.{Cleanup, CleanupFinished}

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
    case msg: Insert   => doInsert(msg)
    case msg: Contains => doContains(msg)
    case msg: Remove   => doRemove(msg)

    case msg: Cleanup         => doCleanup(msg)
    case msg: CleanupFinished => onCleanupFinished(msg)
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
        case None =>
          val ref = context.actorOf(props(m.elem,  initiallyRemoved = false), "test-" + m.elem)
          subtrees = subtrees + (position -> ref)
          m.requester ! OperationFinished(m.id)
      }
      case None => m.requester ! OperationFinished(m.id)
    }
  }

  private def doContains(m: Contains): Unit = {
    getPosition(m.elem) match {
      case Some(position) => subtrees.get(position) match {
        case Some(ref) => ref ! m
        case None => m.requester ! ContainsResult(m.id, result =  false)
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

      case None =>
        removed = true
        m.requester ! OperationFinished(m.id)
    }
  }

  private def cleanupChild(elems: List[Int]): Unit = {
    subtrees.values.take(1).foreach(_ ! Cleanup(elems))
  }

  private def doCleanup(m: Cleanup): Unit = {
    val newElems = if (removed) m.elems else elem :: m.elems

    if (subtrees.nonEmpty) {
      cleanupChild(newElems)
    } else {
      onCleanupFinished(CleanupFinished(newElems))
    }
  }

  private def onCleanupFinished(m: CleanupFinished): Unit = {
    subtrees = subtrees.drop(1)

    if (subtrees.isEmpty) {
      if (!isRoot) {
        context.stop(self)
      }

      context.parent ! m
    } else {
      cleanupChild(m.elems)
    }
  }

  override def postStop(): Unit = {
    subtrees = Map.empty
  }


}
