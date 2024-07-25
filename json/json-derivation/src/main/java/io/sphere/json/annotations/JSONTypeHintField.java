package io.sphere.json.annotations;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.lang.annotation.Inherited;
import static java.lang.annotation.ElementType.TYPE;

/** Specifies name of the field that should be used as a type hint for delegate converters
 *  when deserializing objects that form a class hierarchy. */
@Inherited
@Retention(RetentionPolicy.RUNTIME)
@Target({TYPE})
public @interface JSONTypeHintField {
    String value() default "type";
    String defaultType() default "";
}
