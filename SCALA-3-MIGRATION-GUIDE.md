# Scala 3 migration guide

### How to cross-compile

If you want to compile the same codebase with both scala-2 and scala-3,
you need to port the following methods.

```scala
// JSON derivation
// Why?: These methods are internal and shouldn't be exposed
jsonProduct(CaseClass.apply _) -> deriveJSON
toJsonProduct(CaseClass.apply _) -> deriveToJSON
toJsonProduct0(CaseObject) -> deriveToJSON
fromJsonProduct(CaseClass.apply _) -> deriveFromJSON
fromJsonProduct(CaseObject) -> deriveFromJSON

// deriveJSON dropping Enumeration support
// Why?: There's no derivation required for Enumeration
deriveJSON[SomeEnumeration.Value] -> jsonEnum(Enumeration)

// MongoFormat derivation
// Why?: These methods are internal and shouldn't be exposed
mongoProduct(CaseClass.apply _) -> deriveMongoFormat
mongoProduct0(CaseObject) -> deriveMongoFormat

// TypeSelectorContainer removed
// Why?: TypeSelectorContainer is internal and shouldn't be exposed.
json.asInstanceOf[TypeSelectorContainer].typeSelectors.map(_.typeValue)
->
json.subTypeNames

```

### Deprecated methods

The following methods will not be continued and no alternatives will be provided (because they are not used in our
codebase):

```scala
toJsonSingletonEnumSwitch
fromJsonSingletonEnumSwitch
```


