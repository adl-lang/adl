/* Code generated from adl module test20 */

package adl.test20;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.adl.runtime.Builders;
import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
import org.adl.runtime.Lazy;
import org.adl.runtime.sys.adlast.ScopedName;
import org.adl.runtime.sys.adlast.TypeExpr;
import org.adl.runtime.sys.adlast.TypeRef;
import java.util.ArrayList;
import java.util.Objects;

public class Person {

  /* Members */

  private String firstName;
  private String lastName;
  private short age;
  private Role role;

  /* Constructors */

  public Person(String firstName, String lastName, short age, Role role) {
    this.firstName = Objects.requireNonNull(firstName);
    this.lastName = Objects.requireNonNull(lastName);
    this.age = age;
    this.role = Objects.requireNonNull(role);
  }

  public Person() {
    this.firstName = "";
    this.lastName = "";
    this.age = (short)0;
    this.role = Role.FACTORY.create();
  }

  public Person(Person other) {
    this.firstName = other.firstName;
    this.lastName = other.lastName;
    this.age = other.age;
    this.role = Role.FACTORY.create(other.role);
  }

  /* Accessors and mutators */

  public String getFirstName() {
    return firstName;
  }

  public void setFirstName(String firstName) {
    this.firstName = Objects.requireNonNull(firstName);
  }

  public String getLastName() {
    return lastName;
  }

  public void setLastName(String lastName) {
    this.lastName = Objects.requireNonNull(lastName);
  }

  public short getAge() {
    return age;
  }

  public void setAge(short age) {
    this.age = age;
  }

  public Role getRole() {
    return role;
  }

  public void setRole(Role role) {
    this.role = Objects.requireNonNull(role);
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof Person)) {
      return false;
    }
    Person other = (Person) other0;
    return
      firstName.equals(other.firstName) &&
      lastName.equals(other.lastName) &&
      age == other.age &&
      role.equals(other.role);
  }

  @Override
  public int hashCode() {
    int _result = 1;
    _result = _result * 37 + firstName.hashCode();
    _result = _result * 37 + lastName.hashCode();
    _result = _result * 37 + (int) age;
    _result = _result * 37 + role.hashCode();
    return _result;
  }

  /* Builder */

  public static class Builder {
    private String firstName;
    private String lastName;
    private Short age;
    private Role role;

    public Builder() {
      this.firstName = null;
      this.lastName = null;
      this.age = null;
      this.role = null;
    }

    public Builder setFirstName(String firstName) {
      this.firstName = Objects.requireNonNull(firstName);
      return this;
    }

    public Builder setLastName(String lastName) {
      this.lastName = Objects.requireNonNull(lastName);
      return this;
    }

    public Builder setAge(Short age) {
      this.age = Objects.requireNonNull(age);
      return this;
    }

    public Builder setRole(Role role) {
      this.role = Objects.requireNonNull(role);
      return this;
    }

    public Person create() {
      Builders.checkFieldInitialized("Person", "firstName", firstName);
      Builders.checkFieldInitialized("Person", "lastName", lastName);
      Builders.checkFieldInitialized("Person", "age", age);
      Builders.checkFieldInitialized("Person", "role", role);
      return new Person(firstName, lastName, age, role);
    }
  }

  /* Factory for construction of generic values */

  public static final Factory<Person> FACTORY = new Factory<Person>() {
    @Override
    public Person create() {
      return new Person();
    }

    @Override
    public Person create(Person other) {
      return new Person(other);
    }

    @Override
    public TypeExpr typeExpr() {
      ScopedName scopedName = new ScopedName("test20", "Person");
      ArrayList<TypeExpr> params = new ArrayList<>();
      return new TypeExpr(TypeRef.reference(scopedName), params);
    }
  };

  /* Json serialization */

  public static JsonBinding<Person> jsonBinding() {
    final Lazy<JsonBinding<String>> firstName = new Lazy<>(() -> JsonBindings.STRING);
    final Lazy<JsonBinding<String>> lastName = new Lazy<>(() -> JsonBindings.STRING);
    final Lazy<JsonBinding<Short>> age = new Lazy<>(() -> JsonBindings.INT16);
    final Lazy<JsonBinding<Role>> role = new Lazy<>(() -> Role.jsonBinding());
    final Factory<Person> _factory = FACTORY;

    return new JsonBinding<Person>() {
      @Override
      public Factory<Person> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(Person _value) {
        JsonObject _result = new JsonObject();
        _result.add("fn", firstName.get().toJson(_value.firstName));
        _result.add("ln", lastName.get().toJson(_value.lastName));
        _result.add("age", age.get().toJson(_value.age));
        _result.add("role", role.get().toJson(_value.role));
        return _result;
      }

      @Override
      public Person fromJson(JsonElement _json) {
        JsonObject _obj = JsonBindings.objectFromJson(_json);
        return new Person(
          JsonBindings.fieldFromJson(_obj, "fn", firstName.get()),
          JsonBindings.fieldFromJson(_obj, "ln", lastName.get()),
          JsonBindings.fieldFromJson(_obj, "age", age.get()),
          JsonBindings.fieldFromJson(_obj, "role", role.get())
        );
      }
    };
  }
}
