import io.sphere.mongo.MongoUtils.dbObj
import io.sphere.mongo.format.MongoFormat
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import io.sphere.mongo.format.DefaultMongoFormats._

// This is scala2 only because the only use case for this so far is a limitation of the scala 2 approach and it's easier to use version specific implementations than to port this approach to scala3
class MongoTypeSelectorContainerSpec extends AnyWordSpec with Matchers {
  import MongoTypeSelectorContainerSpec._

  "TypeSelectorContainer should be able to merge traits" in {

    val typeSelectors1 = CartEvent1.mongo.asInstanceOf[MongoTypeSelectorContainer].typeSelectors
    val typeSelectors2 = CartEvent2.mongo.asInstanceOf[MongoTypeSelectorContainer].typeSelectors

    val mergedInstance: MongoFormat[CartEvent] =
      mongoTypeSwitch[CartEvent, CartEvent1](typeSelectors1 ::: typeSelectors2)

    val ce1a = SpecificEvent1A("asd2")
    val ce2b = SpecificEvent2B("asd3")

    SumTypesDerivingSpec.check(
      mergedInstance,
      ce1a,
      dbObj("str" -> "asd2", "type" -> "SpecificEvent1A"))
    SumTypesDerivingSpec.check(
      mergedInstance,
      ce2b,
      dbObj("str" -> "asd3", "type" -> "SpecificEvent2B"))

  }

}

object MongoTypeSelectorContainerSpec {

  sealed trait CartEvent

  sealed trait CartEvent1 extends CartEvent
  case class SpecificEvent1A(str: String) extends CartEvent1
  case class SpecificEvent1B(str: String) extends CartEvent1
  object CartEvent1 {
    implicit val mongo: MongoFormat[CartEvent1] = deriveMongoFormat[CartEvent1]
  }

  sealed trait CartEvent2 extends CartEvent
  case class SpecificEvent2A(str: String) extends CartEvent2
  case class SpecificEvent2B(str: String) extends CartEvent2
  object CartEvent2 {
    implicit val mongo: MongoFormat[CartEvent2] = deriveMongoFormat[CartEvent2]
  }

}
