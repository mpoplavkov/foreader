package ru.poplavkov.foreader

import cats.ApplicativeError
import com.typesafe.scalalogging.StrictLogging
import enumeratum._
import ru.poplavkov.foreader.Logging.{LogLevel, _}

import scala.collection.immutable
import scala.language.higherKinds
import scala.util.control.NonFatal

/**
  * @author mpoplavkov
  */
trait Logging extends StrictLogging {

  implicit class Logged[T](val value: T) {
    def logged(method: String, args: Map[String, Any], level: LogLevel = LogLevel.Debug)
              (toMsg: PartialFunction[T, String] = defaultLogMsg): T = {
      if (toMsg.isDefinedAt(value)) {
        log(s"${methodWithArgs(method, args)}: ${toMsg(value)}", cause = None, level)
      }
      value
    }
  }

  import cats.implicits._

  implicit class LoggedF[F[_], T](val ft: F[T]) {
    def loggedF(method: String, args: Map[String, Any], level: LogLevel = LogLevel.Debug)
               (toMsg: PartialFunction[T, String] = defaultLogMsg)
               (implicit applicativeErr: ApplicativeError[F, Throwable]): F[T] =
      ft.map(_.logged(method, args, level)(toMsg))
        .onError { case NonFatal(e) =>
          log(s"${methodWithArgs(method, args)} failed", cause = Some(e), LogLevel.Error)
          ().pure[F]
        }
  }

  private def log(msg: => String, cause: Option[Throwable], level: LogLevel): Unit =
    cause match {
      case Some(t) => loggingMethodWithCause(level)(msg, t)
      case None => loggingMethod(level)(msg)
    }

  //noinspection ConvertibleToMethodValue
  private def loggingMethod(level: LogLevel): String => Unit =
    level match {
      case LogLevel.Trace => logger.trace(_)
      case LogLevel.Debug => logger.debug(_)
      case LogLevel.Info => logger.info(_)
      case LogLevel.Warn => logger.warn(_)
      case LogLevel.Error => logger.error(_)
    }

  //noinspection ConvertibleToMethodValue
  private def loggingMethodWithCause(level: LogLevel): (String, Throwable) => Unit =
    level match {
      case LogLevel.Trace => logger.trace(_, _)
      case LogLevel.Debug => logger.debug(_, _)
      case LogLevel.Info => logger.info(_, _)
      case LogLevel.Warn => logger.warn(_, _)
      case LogLevel.Error => logger.error(_, _)
    }

}

object Logging {

  private def defaultLogMsg[T]: PartialFunction[T, String] = {
    case value => s"successfully returned $value"
  }

  private def methodWithArgs(method: String, args: Map[String, Any]): String = {
    val argsStr = args
      .map { case (name, value) => s"$name=$value" }
      .mkString(",")
    s"$method($argsStr)"
  }

  sealed trait LogLevel extends EnumEntry

  object LogLevel extends Enum[LogLevel] {

    override def values: immutable.IndexedSeq[LogLevel] = findValues

    case object Trace extends LogLevel

    case object Debug extends LogLevel

    case object Info extends LogLevel

    case object Warn extends LogLevel

    case object Error extends LogLevel

  }

}