package io.sphere.mongo.generic.annotations;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import static java.lang.annotation.ElementType.METHOD;

/** Specifies to embed / flatten all attributes of a nested object into the parent object.
 * This can be used to work around the 22 field limit of case classes without causing unwanted nesting. */
@Retention(RetentionPolicy.RUNTIME)
@Target({METHOD})
public @interface MongoEmbedded {}