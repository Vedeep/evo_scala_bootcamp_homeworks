package com.evobootcamp.homeworks.akka

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout

import java.net.{URLDecoder, URLEncoder}
import java.nio.charset.StandardCharsets
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext}

object Exercise3 extends App {
  case object Passivate

  /*
  Router hosting child actors with identifiers, let's call them "entities":
  - only one entity actor per entity ID maintained
  - router forwards incoming commands to entities based on extractEntityId resolver
  - if extractEntityId is not defined for a command, it should go to unhandled messages
    ( either by receive pattern matching or explicitly by calling unhandled(msg) )
  - entity actor should be started on the first arrival of the command
  - if the entity actor stops itself for some reason, it should be removed from the routing table
    ( context.watch is your friend)
  - entity actors expect their actor names (self.path.name) to be equal to their respective entity IDs
    but sanitized with ActorNameSanitizer (not all characters are allowed in actor names!)
  - on Passivate, router should stop forwarding messages to entities, issue to all the PoisonPill message,
    wait until all terminated and only after that stop itself
   */
  final class EntityRouterActor(
    entityProps: Props,
    extractEntityId: PartialFunction[Any, String],
  ) extends Actor with ActorLogging {

    private var entityRefs: Map[String, ActorRef] = Map.empty

    override def receive: Receive = working

    private def createWorker(id: String): ActorRef =
      entityRefs.getOrElse(id, {
        val ref = context.actorOf(entityProps)
        context.watch(ref)

        entityRefs += (id -> ref)

        ref
      })

    private def removeWorker(ref: ActorRef): Unit =
      entityRefs = entityRefs.filterNot {
        case (_, worker) => worker eq ref
      }

    private def stopAllWorkers(): Unit = entityRefs.values.foreach(_ ! PoisonPill)

    private def tryToStop(): Unit = if (entityRefs.isEmpty) context.stop(self)

    private def working: Receive = {
      case msg @ extractEntityId(id) => createWorker(id).tell(msg, sender)

      case Terminated(worker) => removeWorker(worker)

      case Passivate =>
        stopAllWorkers()
        context.become(passivating)
        tryToStop()

      case msg => unhandled(msg)
    }

    private def passivating: Receive = {
      case Terminated(worker) =>
        removeWorker(worker)
        tryToStop()
    }

    override def preStart(): Unit = {
      log.info("Router starting!")
    }

    override def postStop(): Unit = {
      log.info("Router stopping!")
    }
  }


  object ActorNameSanitizer {
    def sanitize(name: String): String = URLEncoder.encode(name, StandardCharsets.UTF_8)
    def desanitize(name: String): String = URLDecoder.decode(name, StandardCharsets.UTF_8)
  }

  final case class PrintNum(entityId: String, num: Int)
  final case class Done(cmd: PrintNum)

  final class MyEntity extends Actor with ActorLogging {
    private val id = ActorNameSanitizer.desanitize(self.path.name)

    override def preStart(): Unit = {
      log.info("Entity {} starting!", id)
    }

    override def postStop(): Unit = {
      log.info("Entity {} stopping!", id)
    }

    override def receive: Receive = {
      case cmd: PrintNum =>
        log.info("Entity {} received command {}", id, cmd)
        sender() ! Done(cmd)
    }
  }

  implicit val system: ActorSystem = ActorSystem("Exercise3")
  implicit val ec: ExecutionContext = system.dispatcher
  implicit val timeout: Timeout = Timeout(2.seconds)

  val router: ActorRef = system.actorOf(
    Props(new EntityRouterActor(
      entityProps = Props(new MyEntity),
      extractEntityId = {
        case cmd: PrintNum => cmd.entityId
      },
    )),
    "router",
  )

  println(Await.result((router ? PrintNum("e(1)", 10)), timeout.duration))
  println(Await.result((router ? PrintNum("e(2)", 20)), timeout.duration))
  println(Await.result((router ? PrintNum("e(1)", 30)), timeout.duration))
  router ! Passivate
}
