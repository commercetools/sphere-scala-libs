package io.sphere.json.annotations;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import static java.lang.annotation.ElementType.TYPE;

/** Specifies a type hint used to choose the correct type when deserializing
 *  types that form a type hierarchy using a delegate converter. */
@Retention(RetentionPolicy.RUNTIME)
@Target({TYPE})
public @interface JSONTypeHint {
    String value() default "";
}