package io.sphere.mongo.generic.annotations;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import static java.lang.annotation.ElementType.METHOD;

/** Specifies the field name to use in Mongo, instead of defaulting to the field name of the case class. */
@Retention(RetentionPolicy.RUNTIME)
@Target({METHOD})
public @interface MongoKey {
    String value();
}