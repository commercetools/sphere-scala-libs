package io.sphere.util

trait Logging extends com.typesafe.scalalogging.slf4j.Logging {
  val log = logger
}
