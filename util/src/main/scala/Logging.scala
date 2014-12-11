package io.sphere.util

trait Logging extends com.typesafe.scalalogging.slf4j.StrictLogging {
  val log = logger
}
