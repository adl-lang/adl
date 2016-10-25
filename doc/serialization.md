# Serialization

In order that ADL data structures serialized in one language can be
deserialized in other, the mapping to serialized values must be
consistent. Hence, ADL is a formal specification for serialized data.

Currently json serialization is implemented, as this gives best
interoperability with browser based code.  It is envisaged that in
future binary serialization will allow be supported. Note also that
json serialisation is also format used for literals in the ADL
language itself.

Each ADL primitive and declaration type is serialized as specified in
the rules below. The serialization for any ADL type can hence be
determined by the repeated application of these rules.

# Specification

## Primitives

| Type                         | JSON Value                                             |
|------------------------------|--------------------------------------------------------|
| Int8,Int16,Int32             | number                                                 |
| Word8,Word16,Word32          | number                                                 |
| Int64,Word64                 | number [^1]                                            |
| Bool                         | true or false                                          |
| Void                         | null                                                   |
| Float,Double                 | number                                                 |
| String                       | string                                                 |
| ByteArray                    | string containing base 64 encoded data                 |
| Vector<T>                    | json array containing serialized values of type T      |
| StringMap<T>                 | A json object containing values of type T              |

[^1]: It's not clear that a json number is the best representation for
64 bit values. The [json specification][jsonspec] doesn't specify a
precision for numbers, but javascript implementations typically
represent all numbers as a double, which lacks the precision to
represent all Int32 values.

## Declarations

### Structs

An ADL `struct` has the obvious json representation - name a
json object where each field has the name of the ADL field, and
corresponding value is the serialized representation the ADL fields
type, ie

```
struct F
{
    Int32 field1;
    Vector<String> field2;
}
```

would allow the json value:

```
{
    "field1" : 42,
    "field2" : ["the","day","is","done"]
}
```

### Unions

An ADL `union` is a sum type - only one field can be active at any
time. The json serialisation is thus an object with a single name /value
pair: the name of the active field, and it's serialized value.

Hence

```
union F
{
    Int32 field1;
    Vector<String> field2;
}
```

could have have either field active, (but not both),  eg

```
{ "field1" : 42 }
```

or

```
{ "field2" : ["the","day","is","done"] }
```

### Newtypes

An ADL `newtype` has exactly the same serialisation as the
underlying type ie:

```
newtype ScopedName = Vector<String>;
```

would have a representation like

```
["org","adl","ast"]
```

### Generics

Only monomophic values can be constructed, and hence actually be serialized.
The json serialized values match the serialization of the generic type with
type parameters fully substituted. ie with a definition

```
union Maybe<T>
{
   T just;
   Void nothing;
};
```

The following would be valid json values for a type `Maybe<Vector<String>>`:

```
{ "nothing" : nullify }

{ "just" : ["Sydney","Melbourne","Darwin"] }
```





