A key feature of ADL is it's support for custom type mappings.

When a type is defined it ADL, a (language independent) serialisation
specification is implied. Running the compiler with a given language
target will generate definitions for that type, as well the necessary
serialisation code. The generated definitions correspond to how one
would naturally write that type in the target language. However, often
one would like to use an equivalent existing type, for compatibility
with other code. Custom types make this possible.

As an example, consider a date type. There is no primitive in the ADL
language for dates, so we need to define one. A possible ADL definition is:

```
newtype Date = String = "1900-01-01";   // dates are ISO-8601 strings.
```

This says that a Date is a new independent type, isomorphic to a
string. The default date is 1 Jan 1900. This would result in the following
generated haskell code:

```Haskell
newtype Date = Date { unDate :: T.Text }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue Date where ...
```
The issue here is that all user written code making use of ADL values
must be prepared to convert from date strings to more appropriate
native types. The solution is to use a custom type mapping to map the
ADL Date definition to the [Day][1] type from the standard
library. This means that, in all generated haskell code, Day will be
used in lieu of the Date declaration shown above. The developer no
longer need to write any conversion code - all dates throughout the
ADL system use the library type of her choice.

Of course, the custom type mapping is language specific. When
generating java code, one may elect to map dates to, say,
[org.joda.time.LocalDate][2].

The key thing to realise here is that, even when custom type mappings
are being used, the ADL definitions fix the serialisation of the data
types. This is what permits interoperability between processes coded
in different languages.

Custom type mappings can be valuable for more complex types
also. Consider the ADL definition:

```
struct Contact
{
    String fullName;
    String address;
    Date dateOfBirth;
};
```
which would result in the following generated C++ code:

```C++
struct Contact
{
    Contact();
    
    Contact(
        const std::string & fullName,
        const std::string & address,
        const Date & dateOfBirth
        );
    
    std::string fullName;
    std::string address;
    Date dateOfBirth;
};

bool operator==( const Contact &a, const Contact &b );
bool operator<( const Contact &a, const Contact &b );

template <> struct Serialisable<ADL::Contact> { ... };
```
Whilst this is a sensible definition, it may be that, within our
C++ system, we need this to implement some internal API, perhaps:

```c++
class DBObject
{
public:
    virtual void writeToDB( DBConnection & ) = 0;
};
```

Custom types make this possible. The developer would write a class


```c++
struct MyContact : public DBObject
{
    virtual void writeToDB( DBConnection & );

    Contact_ADL contact;
};
```

and then provide a custom type mapping to the C++ code generator that
will use MyContact everywhere it would have used Contact, but make the
original generated Contact code available under the name Contact_ADL,
for use in the implementation of our custom type.

Custom type mappings are used in the ADL standard library. The
following custom mappings are in place:

| ADL Type     | Haskell Type   | C++ Type  | Java Type          |
|--------------|----------------|-----------|--------------------|
| Pair<T1,T2>  | (,)            | std::pair |                    |
| Map<K,V>     | Data.Map.Map   | std::set  | java.util.HashMap  |
| Set<V>       | Data.Set.Set   | std::map  | java.util.HashSet  |
| Maybe<T>     | prelude.Maybe  |           | java.util.Optional |
| Either<T1,T> | prelude.Either |           |                    |


[1]: http://hackage.haskell.org/package/time-1.1.2.1/docs/Data-Time-Calendar.html#t%3ADay
[2]: http://www.joda.org/joda-time/apidocs/org/joda/time/LocalDate.html





