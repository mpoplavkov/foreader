package ru.poplavkov

import com.softwaremill.tagging._

package object foreader {

  trait TokenTag
  type Token = String @@ TokenTag

}
