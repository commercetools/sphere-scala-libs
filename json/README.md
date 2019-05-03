# sphere-json

Typeclasses & derived instances on top of [json4s](http://json4s.org/).

![Cats Friendly Badge](https://typelevel.org/cats/img/cats-badge-tiny.png) 

## Motivation

sphere-json was created with the following goals / requirements:

  - A typeclass-based approach for flexibility (easily define serialization formats for 3rd party types)
    and type-safety (look for / pick "serializers" at compile-time)
  - A primary focus on product and sum types (case classes/objects and sealed hierarchies of these)
  - Easy accumulation of errors on deserialization of case classes with multiple fields
  - Minimal to no use of reflection, especially not during the actual (de-)serialization process
  - Due to facing hundreds of data types and thousands of fields, a way to "derive"
    standard instances without much boilerplate.

Therefore, you might want to use this library if:

  - You are already using json4s and intend to keep using it.
  - You are almost exclusively using case classes / objects for your serializable data types.
  - You want a typeclass-based approach for JSON serialization.
  - You want to accumulate errors.
  - You are dealing with hundreds of data types and many more fields to serialize.
  - The JSON produced by the derived instances suits your needs.

## Repository

Until the artifacts are released to Maven Central, please use our public repo:

    resolvers += Resolver.bintrayRepo("commercetools", "maven")

    libraryDependencies += "io.sphere" %% "sphere-json" % "0.9.26"

## Basic Usage

sphere-json defines three simple type classes in the form of the following traits:

    import cats.data.ValidatedNel
    trait FromJSON[A] {
      def read(jval: JValue): ValidatedNel[JSONError, A]
    }
    trait ToJSON[A] {
      def write(value: A): JValue
    }
    trait JSON[A] extends FromJSON[A] with ToJSON[A]

The core API are two methods from the `io.sphere.json` package:

    import cats.data.ValidatedNel
    def fromJSON[A: FromJSON](json: String): ValidatedNel[JSONError, A]
    def toJSON[A: ToJSON](a: A): String

To use your own types with these methods you need to define the corresponding type class instances
and make sure they're either in implicit scope when calling these methods or by passing them in
manually. A typical place for defining typeclass instances is in the companion objects of the types
themselves:

    case class User(name: String, age: Int, location: String)
    object User {
      implicit val json: JSON[User] = new JSON[User] { ... }
    }

You can implement these instances completely by hand, which provides the most flexibility but can
be a bit tedious and repetitive in standard cases. To help with applicative-style parsing of fields,
there is another method available from the same package:

    def field[A: FromJSON](name: String, default: Option[A] = None)(jval: JValue): ValidatedNel[JSONError, A]

It can be used in an implementation of `read` as follows:

    implicit val json: JSON[User] = new JSON[User] {
      import cats.data.ValidatedNel
      import cats.syntax.apply._

      def read(jval: JValue): ValidatedNel[JSONError, User] = jval match {
        case o: JObject =>
          (field[String]("name")(o),
           field[Int]("age")(o),
           field[String]("location")(o)).mapN(User.apply)
        case _ => fail("JSON object expected.")
      }

      def write(u: User): JValue = JObject(List(
        "name" → toJValue(u.name),
        "age" → toJValue(u.age),
        "location" → toJValue(u.location)
      ))
    }
  }

## using `ToJSON.instance`

```
import java.util.UUID

case class User(id: UUID, firstName: String, age: Int)

import io.sphere.json._

implicit val encodeUser: ToJSON[User] = ToJSON.instance[User](u ⇒ JObject(List(
  "id" → toJValue(u.id),
  "first_name" → toJValue(u.firstName),
  "age" → toJValue(u.age)
)))

val id = UUID.randomUUID()
val json = toJValue(User(UUID.randomUUID(), "bidule", 109))
```

## Deriving Instances for `JSON`

sphere-json comes with a few functions that generically derive `JSON` instances.
These functions employ some reflection to get the field names and their default values,
however, the results of gathering this information are memoized and it only applies during
creation of the actual JSON instance, not during subsequent (de)serialization using that instance.

All of the following functionality is provided by the `io.sphere.json.generic` package. The most general
way to get a derived `JSON` instance for some type is to use the `deriveJSON` macro:

    implicit val json: JSON[MyType] = deriveJSON[MyType]

The `deriveJSON` macro expands to a call of one of several underlying generic methods
that can also be used directly and are described further below.

The `read` implementations of derived instances always use applicative-style parsing
(be using applicative builders via `|@|`) via the `Validated` type
provided by cats for error accumulation. The `write` implementations take the values
out of the case class instances via the compiler-generated `productElement(i)` methods,
so there is no reflection involved during the actual (de-)serialization process of
derived instances.

> Note that at this time only `JSON` instances can be generically derived, not separate
> `FromJSON` or `ToJSON` instances. However, it is easy to expose a `JSON` instance as only
> a `FromJSON` or `ToJSON` by typing the implicit definition appropriately.

### Case Classes (product types)

Given

    case class Foo(x: String, y: Int)

an instance defined as

    implicit val json = deriveJSON[Foo]

expands to

    implicit val json = jsonProduct(Foo.apply _)

`jsonProduct` is an overloaded method that, when given a constructor function
`(X1, ..., XN) => X` where `X <: Product` and `X1` to `XN` are the argument types,
creates a `JSON[X]` instance.

Case classes using such a derived instance are serialized as JSON objects.

### Singleton Objects

Given

    object Foo

an instance defined as

    implicit val json = deriveJSON[Foo.type]

expands to

    implicit val json = jsonSingleton(Foo)

The derived instance serializes the singleton object as a string (the simple class name
minus any `$` signs by default).

### Sealed Traits / Abstract Classes (sum types)

Given

    sealed abstract class X
    case class A(a: Int) extends X
    case class B(b: String) extends X
    case object C extends X

an instance defined as

    implicit val json = deriveJSON[X]

Expands to:

    implicit val json = {
      implicit val json0 = jsonProduct(A.apply _)
      implicit val json1 = jsonProduct(B.apply _)
      implicit val json2 = jsonProduct0(C)
      jsonTypeSwitch[X, A, B, C.type](Nil)
    }

As can be seen in the example, when using `deriveJSON` on a `sealed` type,
the instances for all the other types that make up the sum type are automatically
defined in a nested scope and will only be visible to the final `jsonTypeSwitch`
invocation, which creates a `JSON[X]` instance that adds a `type` field to the JSON
on serialization that is subsequently used on deserialization to delegate to the right
typeclass instance of either `A`, `B` or `C`.

All members of such a sum type are serialized as JSON objects with a `type` field whose
values are the simple class names (minus any `$` signs) of the classes involved.
The name of this extra field as well as the values used for each type can be configured.

### Enumerations

Given

    object MyEnum extends Enumeration {
      val One, Two, Three = Value
    }

an instance defined as

    implicit val json = deriveJSON[MyEnum.Value]

expands to

    implicit val json = jsonEnum(MyEnum)


### Configuration

Some details of the behavior of derived JSON instances for product and sum types can be configured via annotations:

 * `@JSONKey([name])` - Applied on a case class constructor field. Specifies the field name to use in JSON,
               instead of defaulting to the field name of the case class.
 * `@JSONIgnore` - Applied on a case class constructor field. Specifies to ignore a certain field on serialization.
                  The field *must* have a default value.
 * `@JSONEmbedded` - Applied on a case class constructor field whose type is another case class.
                    Specifies to embed / flatten all attributes of the nested object
                    into the parent object in JSON. This can be used to work around the 22 field
                    limit of case classes without causing unwanted nesting in the JSON and in general
                    allows your case class nesting to diverge from the JSON object nesting without
                    having to give up the derived instances.
 * `@JSONTypeHintField`([name]) - Applied on a sealed trait, sealed abstract class or case class.
                         Specifies that an extra type field should be added to the JSON,
                         optionally overriding the default name of that field, which is `type`.
                         Note that you typically only need to use this on a sealed type if you
                         want to change the name of the field as derived instances for
                         sealed types always require and add the extra type field.
                         When applied on a case class that is otherwise not part of a sealed type
                         and thus would normally not have a type field added by the derived
                         JSON instance, it forces a type field to always be written for that
                         class anyway, so you can use that if you want to have the extra type field
                         in the JSON of your case class regardless of whether it is actually needed
                         by a sum type deserializer.
 * `@JSONTypeHint(value)` - Applied on a case class or case object. Specifies the value to write to the
                    type field in JSON. If specified, a type field is always added, so you never
                    need to specify both, `JSONTypeHintField` and `JSONTypeHint` on the same type.

Should you need even more customization for a specific type, you are better off writing the
JSON instance(s) for that type(s) by hand. The derived instances are mere convenience. For us they
cover ~80% of the cases and the rest are specially handcrafted instances due to very specific
(de)serialization requirements or limitations of the current derivation process.

> Using annotations for that kind of customization is surely not ideal and defeats one
> of the advantages of typeclasses as you cannot retroactively add annotations to types
> that you don't own. Deriving instances for such types without any customized configuration
> works fine, however. The use of annotations here for customizing the derived instances is more
> of an old wart that is hopefully removed in the future.

### Reuse of Derived Instances

A case that comes up every now and then is the need to add one or more computed `val`s of a case class or object
to the JSON representation that are not part of the constructor. A solution that can be applied in these and
similar cases without having to skip the derived instance completely is this:

    case class X(a: Int, b: String) {
      val c = a * 10
    }
    object X {
      private val _json = deriveJSON[X]
      implicit val json = new JSON[X] {
        def read(j: JValue) = _json.read(j)
        def write(x: X) = _json.write(x) ++ JField("c", toJValue(x.c))
      }
    }

### Common Pitfalls

Here are some common pitfalls that you might encounter with the current derived instances.

### Default Values

Please note that the default values from case class constructors are obtained only once.
In other words, only a single default value is obtained for each field once when reflecting
on the case class constructor arguments. This is by design but can trip you up if you're
using "dynamic" default values, (e.g. x: DateTime = new DateTime()). We generally discourage
doing that. Case class constructors should be pure functions.

## Dependencies

 * json4s
 * sphere-util
   * cats
   * joda-time
   * slf4j

## License

Apache 2.0 license.

## Credits

The basic typeclass-based functionality of this library outside of the `io.sphere.json.generic` package
is mostly just a repetition of what most other typeclass-based JSON implementations are doing, it has the
most in common with `lift-json-scalaz` due to sharing the approach to error-acumulation provided by scalaz's
Validation data type and its applicative instance.

## Other Projects <a id="other-projects"></a>

A lot has happened in the Scala JSON landscape since this project was initially created internally and there are now
plenty of options to choose from, many of which are very similar. For that reason, it is worth mentioning some
other projects that you might want to evaluate:

  * [circe](https://circe.github.io/circe/):
    Typeclasses and deriving instances with shapeless which are very similar to sphere-json, also based on cats.

  * [lift-json-scalaz](https://github.com/lift/framework/commits/master/core/json-scalaz):
    Typeclasses and some utility functions on top of lift-json that integrate with some parts of scalaz.

 * [sjson](https://github.com/debasishg/sjson):
    Provides reflection and typeclass-based serialization as two alternatives. Also comes with
    generic functions for deriving typeclass instances for case classes.

 * [argonaut](http://argonaut.io/):
    Also using typeclasses. Very well integrated with scalaz, includes a zipper, (partial) lenses and much more.

 * [play-json](https://github.com/playframework/play-json):
    Since version 2.0/2.1, typeclasses and macros for deriving instances which are very
    similar to sphere-json.

 * [spray-json](https://github.com/spray/spray-json):
    Yet again typeclasses and generic functions for deriving instances for case classes.
    Also very similar to sphere-json.

 * [salat](https://github.com/novus/salat):
    Not primarily for JSON but focused on getting case class instances in and out of MongoDB.
    JSON serialization is an additional feature. No typeclasses.


