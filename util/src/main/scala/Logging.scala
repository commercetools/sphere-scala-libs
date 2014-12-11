package io.sphere.util

trait Logging extends com.typesafe.scalalogging.slf4j.LazyLogging {
  val log = logger
}
