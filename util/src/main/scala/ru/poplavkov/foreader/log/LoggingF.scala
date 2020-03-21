package ru.poplavkov.foreader.log

import cats.data.OptionT
import cats.syntax.applicative._
import cats.syntax.applicativeError._
import cats.syntax.functor._
import ru.poplavkov.foreader.ApplicativeErrorSupport
import ru.poplavkov.foreader.log.Logging._

import scala.language.higherKinds
import scala.util.control.NonFatal

/**
  * @author mpoplavkov
  */
trait LoggingF[F[_]] extends Logging with ApplicativeErrorSupport[F] {

  implicit class LoggedF[T](val ft: F[T]) {
    def loggedF(method: String, args: Map[String, Any], level: LogLevel = LogLevel.Debug)
               (toMsg: PartialFunction[T, String] = defaultLogMsg): F[T] =
      ft.map(_.logged(method, args, level)(toMsg))
        .onError { case NonFatal(e) =>
          log(s"${methodWithArgs(method, args)} failed", cause = Some(e), LogLevel.Error)
          ().pure[F]
        }
  }

  implicit class OptionTLoggedF[T](val optionT: OptionT[F, T]) {
    def loggedF(method: String, args: Map[String, Any], level: LogLevel = LogLevel.Debug)
               (toMsg: PartialFunction[Option[T], String] = defaultLogMsg): OptionT[F, T] =
      OptionT(optionT.value.loggedF(method, args, level)(toMsg))
  }

}
