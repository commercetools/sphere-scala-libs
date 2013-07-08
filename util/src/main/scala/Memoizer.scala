package io.sphere.util

import java.util.concurrent._

/** Straight port from the Java impl. of "Java Concurrency in Practice". */
final class Memoizer[K, V](action: K => V) extends (K => V) {
  private val cache = new ConcurrentHashMap[K, Future[V]]
  def apply(k: K): V = {
    while (true) {
      var f = cache.get(k)
      if (f == null) {
        val eval = new Callable[V] { def call(): V = action(k) }
        val ft = new FutureTask[V](eval)
        f = cache.putIfAbsent(k, ft)
        if (f == null) {
          f = ft
          ft.run()
        }
      }
      try { return f.get } catch {
        case e: CancellationException => cache.remove(k, f)
        case e: ExecutionException => throw e.getCause
      }
    }
    sys.error("Failed to compute result.")
  }
}