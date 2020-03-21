package ru.poplavkov.foreader

import com.typesafe.scalalogging.StrictLogging
import enumeratum._
import ru.poplavkov.foreader.Logging.LogLevel

import scala.collection.immutable

/**
  * @author mpoplavkov
  */
trait Logging extends StrictLogging {

  implicit class Logged[T](val value: T) {
    def logged(method: String, level: LogLevel = LogLevel.Debug)
              (toMsg: PartialFunction[T, String]): T = {
      if (toMsg.isDefinedAt(value)) {
        log(s"$method: ${toMsg(value)}", cause = None, level)
      }
      value
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