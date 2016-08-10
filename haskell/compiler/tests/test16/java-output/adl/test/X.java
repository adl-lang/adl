package adl.test;

import adl.test2.A;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import java.util.Objects;

public class X {

  /* Members */

  private A value;

  /* Constructors */

  public X(A value) {
    this.value = Objects.requireNonNull(value);
  }

  public X() {
    this.value = new A();
  }

  public X(X other) {
    this.value = A.FACTORY.create(other.value);
  }

  /* Accessors and mutators */

  public A getValue() {
    return value;
  }

  public void setValue(A newValue) {
    value = Objects.requireNonNull(newValue);
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof X)) {
      return false;
    }
    X other = (X) other0;
    return
      value.equals(other.value);
  }

  @Override
  public int hashCode() {
    int result = 1;
    result = result * 37 + value.hashCode();
    return result;
  }

  /* Factory for construction of generic values */

  public static final Factory<X> FACTORY = new Factory<X>() {
    public X create() {
      return new X();
    }
    public X create(X other) {
      return new X(other);
    }
  };

  /* Json serialization */

  public static JsonBinding<X> jsonBinding() {
    final JsonBinding<A> value = A.jsonBinding();
    final Factory<X> _factory = FACTORY;

    return new JsonBinding<X>() {
      public Factory<X> factory() {
        return _factory;
      }

      public JsonElement toJson(X _value) {
        JsonObject _result = new JsonObject();
        _result.add("value", value.toJson(_value.value));
        return _result;
      }

      public X fromJson(JsonElement _json) {
        JsonObject _obj = _json.getAsJsonObject();
        return new X(
          _obj.has("value") ? value.fromJson(_obj.get("value")) : new A()
        );
      }
    };
  }
}
