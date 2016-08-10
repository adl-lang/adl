package org.adl.sys.types;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import java.util.Objects;

public class Pair<T1, T2> {

  /* Members */

  private T1 v1;
  private T2 v2;

  /* Constructors */

  public Pair(T1 v1, T2 v2) {
    this.v1 = Objects.requireNonNull(v1);
    this.v2 = Objects.requireNonNull(v2);
  }

  /* Accessors and mutators */

  public T1 getV1() {
    return v1;
  }

  public void setV1(T1 newV1) {
    v1 = Objects.requireNonNull(newV1);
  }

  public T2 getV2() {
    return v2;
  }

  public void setV2(T2 newV2) {
    v2 = Objects.requireNonNull(newV2);
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof Pair)) {
      return false;
    }
    Pair other = (Pair) other0;
    return
      v1.equals(other.v1) &&
      v2.equals(other.v2);
  }

  @Override
  public int hashCode() {
    int result = 1;
    result = result * 37 + v1.hashCode();
    result = result * 37 + v2.hashCode();
    return result;
  }

  /* Factory for construction of generic values */

  public static <T1, T2> Factory<Pair<T1, T2>> factory(Factory<T1> factoryT1, Factory<T2> factoryT2) {
    return new Factory<Pair<T1, T2>>() {
      final Factory<T1> v1 = factoryT1;
      final Factory<T2> v2 = factoryT2;

      public Pair<T1, T2> create() {
        return new Pair<T1, T2>(
          v1.create(),
          v2.create()
          );
      }

      public Pair<T1, T2> create(Pair<T1, T2> other) {
        return new Pair<T1, T2>(
          v1.create(other.getV1()),
          v2.create(other.getV2())
          );
      }
    };
  }

  /* Json serialization */

  public static<T1, T2> JsonBinding<Pair<T1, T2>> jsonBinding(JsonBinding<T1> bindingT1, JsonBinding<T2> bindingT2) {
    final JsonBinding<T1> v1 = bindingT1;
    final JsonBinding<T2> v2 = bindingT2;
    final Factory<T1> factoryT1 = bindingT1.factory();
    final Factory<T2> factoryT2 = bindingT2.factory();
    final Factory<Pair<T1, T2>> _factory = factory(bindingT1.factory(), bindingT2.factory());

    return new JsonBinding<Pair<T1, T2>>() {
      public Factory<Pair<T1, T2>> factory() {
        return _factory;
      }

      public JsonElement toJson(Pair<T1, T2> _value) {
        JsonObject _result = new JsonObject();
        _result.add("v1", v1.toJson(_value.v1));
        _result.add("v2", v2.toJson(_value.v2));
        return _result;
      }

      public Pair<T1, T2> fromJson(JsonElement _json) {
        JsonObject _obj = _json.getAsJsonObject();
        return new Pair<T1, T2>(
          _obj.has("v1") ? v1.fromJson(_obj.get("v1")) : factoryT1.create(),
          _obj.has("v2") ? v2.fromJson(_obj.get("v2")) : factoryT2.create()
        );
      }
    };
  }
}
