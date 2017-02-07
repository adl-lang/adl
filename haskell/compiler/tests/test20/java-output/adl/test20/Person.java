package adl.test20;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
import org.adl.runtime.Lazy;
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
    int result = 1;
    result = result * 37 + firstName.hashCode();
    result = result * 37 + lastName.hashCode();
    result = result * 37 + (int) age;
    result = result * 37 + role.hashCode();
    return result;
  }

  /* Factory for construction of generic values */

  public static final Factory<Person> FACTORY = new Factory<Person>() {
    public Person create() {
      return new Person();
    }
    public Person create(Person other) {
      return new Person(other);
    }
  };

  /* Json serialization */

  public static JsonBinding<Person> jsonBinding() {
    final Lazy<JsonBinding<String>> firstName = new Lazy<>(() -> JsonBindings.STRING);
    final Lazy<JsonBinding<String>> lastName = new Lazy<>(() -> JsonBindings.STRING);
    final Lazy<JsonBinding<Short>> age = new Lazy<>(() -> JsonBindings.SHORT);
    final Lazy<JsonBinding<Role>> role = new Lazy<>(() -> Role.jsonBinding());
    final Factory<Person> _factory = FACTORY;

    return new JsonBinding<Person>() {
      public Factory<Person> factory() {
        return _factory;
      }

      public JsonElement toJson(Person _value) {
        JsonObject _result = new JsonObject();
        _result.add("fn", firstName.get().toJson(_value.firstName));
        _result.add("ln", lastName.get().toJson(_value.lastName));
        _result.add("age", age.get().toJson(_value.age));
        _result.add("role", role.get().toJson(_value.role));
        return _result;
      }

      public Person fromJson(JsonElement _json) {
        JsonObject _obj = _json.getAsJsonObject();
        return new Person(
          _obj.has("fn") ? firstName.get().fromJson(_obj.get("fn")) : "",
          _obj.has("ln") ? lastName.get().fromJson(_obj.get("ln")) : "",
          _obj.has("age") ? age.get().fromJson(_obj.get("age")) : (short)0,
          _obj.has("role") ? role.get().fromJson(_obj.get("role")) : Role.FACTORY.create()
        );
      }
    };
  }
}
