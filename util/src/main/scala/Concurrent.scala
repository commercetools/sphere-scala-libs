package io.sphere.util

import java.util.concurrent.ThreadFactory
import java.util.concurrent.atomic.AtomicInteger

object Concurrent {
  def namedThreadFactory(poolName: String): ThreadFactory =
    new ThreadFactory {
      val count = new AtomicInteger(0)
      override def newThread(r: Runnable) =
        new Thread(r, poolName + "-" + count.incrementAndGet)
    }
}
