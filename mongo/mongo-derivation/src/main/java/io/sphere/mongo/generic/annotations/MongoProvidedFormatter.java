package io.sphere.mongo.generic.annotations;

import java.lang.annotation.Target;

import static java.lang.annotation.ElementType.TYPE;

/**
 * A subtype marked with MongoProvidedFormatter will use the custom provided formatter instead of the automatically generated one.
 */
@Target({TYPE})
public @interface MongoProvidedFormatter {
}