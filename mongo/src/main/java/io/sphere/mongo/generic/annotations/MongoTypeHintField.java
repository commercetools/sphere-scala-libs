package io.sphere.mongo.generic.annotations;

import java.lang.annotation.Inherited;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import static java.lang.annotation.ElementType.TYPE;

/** Specifies name of the field that should be used as a type hint for delegate converters
 *  when deserializing objects that form a class hierarchy. */
@Inherited
@Retention(RetentionPolicy.RUNTIME)
@Target({TYPE})
public @interface MongoTypeHintField {
    String value() default "type";
}