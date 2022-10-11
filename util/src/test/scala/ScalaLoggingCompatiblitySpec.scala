import com.typesafe.scalalogging.Logger
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers

class ScalaLoggingCompatiblitySpec extends AnyFunSpec with Matchers {

  describe("Ensure we skip ScalaLogging 3.9.5, because varargs will not compile under 3.9.5") {
    // Github issue about the bug: https://github.com/lightbend-labs/scala-logging/issues/354
    // This test can be removed if it compiles with scala-logging versions bigger than 3.9.5
    object Log extends com.typesafe.scalalogging.StrictLogging {
      val log: Logger = logger
    }
    val list: List[AnyRef] = List("log", "Some more")

    Log.log.warn("Message1", list: _*)
  }

}
