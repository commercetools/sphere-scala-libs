package io.sphere.json.annotations;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import static java.lang.annotation.ElementType.METHOD;

/** Specifies to ignore a certain field on serialization. The field must have a default value. */
@Retention(RetentionPolicy.RUNTIME)
@Target({METHOD})
public @interface JSONIgnore {}