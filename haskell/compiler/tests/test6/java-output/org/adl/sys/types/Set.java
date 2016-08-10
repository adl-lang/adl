package org.adl.sys.types;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
import java.util.ArrayList;
import java.util.Objects;

public class Set<T> {

  /* Members */

  private ArrayList<T> value;

  /* Constructors */

  public Set(ArrayList<T> value) {
    this.value = Objects.requireNonNull(value);
  }

  /* Accessors and mutators */

  public ArrayList<T> getValue() {
    return value;
  }

  public void setValue(ArrayList<T> newValue) {
    value = Objects.requireNonNull(newValue);
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof Set)) {
      return false;
    }
    Set other = (Set) other0;
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

  public static <T> Factory<Set<T>> factory(Factory<T> factoryT) {
    return new Factory<Set<T>>() {
      final Factory<ArrayList<T>> value = Factories.arrayList(factoryT);

      public Set<T> create() {
        return new Set<T>(
          value.create()
          );
      }

      public Set<T> create(Set<T> other) {
        return new Set<T>(
          value.create(other.getValue())
          );
      }
    };
  }

  /* Json serialization */

  public static<T> JsonBinding<Set<T>> jsonBinding(JsonBinding<T> bindingT) {
    final JsonBinding<ArrayList<T>> value = JsonBindings.arrayList(bindingT);
    final Factory<T> factoryT = bindingT.factory();
    final Factory<Set<T>> _factory = factory(bindingT.factory());

    return new JsonBinding<Set<T>>() {
      public Factory<Set<T>> factory() {
        return _factory;
      }

      public JsonElement toJson(Set<T> _value) {
        JsonObject _result = new JsonObject();
        _result.add("value", value.toJson(_value.value));
        return _result;
      }

      public Set<T> fromJson(JsonElement _json) {
        JsonObject _obj = _json.getAsJsonObject();
        return new Set<T>(
          _obj.has("value") ? value.fromJson(_obj.get("value")) : new ArrayList<T>()
        );
      }
    };
  }
}
