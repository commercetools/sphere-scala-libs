package io.sphere.mongo
import com.mongodb.BasicDBObject

object MongoUtils {

  def dbObj(pairs: (String, Any)*) =
    pairs.foldLeft(new BasicDBObject){case (obj, (key, value)) => obj.append(key, value)}

}
