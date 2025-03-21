package io.sphere.json

import com.fasterxml.jackson.databind.DeserializationFeature.{
  USE_BIG_DECIMAL_FOR_FLOATS,
  USE_BIG_INTEGER_FOR_INTS
}
import com.fasterxml.jackson.databind.ObjectMapper
import org.json4s.jackson.{Json4sScalaModule, JsonMethods}

// extends the default JsonMethods to configure a different default jackson parser
private object SphereJsonParser extends JsonMethods {
  override val mapper: ObjectMapper = {
    val m = new ObjectMapper()
    m.registerModule(new Json4sScalaModule)
    m.configure(USE_BIG_INTEGER_FOR_INTS, false)
    m.configure(USE_BIG_DECIMAL_FOR_FLOATS, false)
    m
  }
}
