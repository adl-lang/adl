package org.adl.sys.types;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
import java.util.Map;
import java.util.Objects;

public class Error<T> {

  /* Members */

  private Disc disc;
  private Object value;

  /**
   * The Error discriminator type.
   */
  public enum Disc {
    VALUE,
    ERROR
  }

  /* Constructors */

  public static <T> Error<T> value(T v) {
    return new Error<T>(Disc.VALUE, Objects.requireNonNull(v));
  }

  public static <T> Error<T> error(String v) {
    return new Error<T>(Disc.ERROR, Objects.requireNonNull(v));
  }

  private Error(Disc disc, Object value) {
    this.disc = disc;
    this.value = value;
  }

  /* Accessors */

  public Disc getDisc() {
    return disc;
  }

  public T getValue() {
    if (disc == Disc.VALUE) {
      return Error.<T>cast(value);
    }
    throw new IllegalStateException();
  }

  public String getError() {
    if (disc == Disc.ERROR) {
      return (String) value;
    }
    throw new IllegalStateException();
  }

  /* Mutators */

  public void setValue(T v) {
    this.value = Objects.requireNonNull(v);
    this.disc = Disc.VALUE;
  }

  public void setError(String v) {
    this.value = Objects.requireNonNull(v);
    this.disc = Disc.ERROR;
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof Error)) {
      return false;
    }
    Error other = (Error) other0;
    return disc == other.disc && value.equals(other.value);
  }

  @Override
  public int hashCode() {
    return disc.hashCode() * 37 + value.hashCode();
  }

  @SuppressWarnings("unchecked")
  private static <T> T cast(final Object o) {
    return (T) o;
  }

  /* Factory for construction of generic values */

  public static <T> Factory<Error <T>> factory(Factory<T> factoryT) {
    return new Factory<Error<T>>() {
      final Factory<T> value = factoryT;
      final Factory<String> error = Factories.STRING;

      public Error<T> create() {
        return new Error<T>(Disc.VALUE,value.create());
      }

      public Error<T> create(Error<T> other) {
        switch (other.disc) {
          case VALUE:
            return new Error<T>(other.disc,value.create(Error.<T>cast(other.value)));
          case ERROR:
            return new Error<T>(other.disc,other.value);
        }
        throw new IllegalArgumentException();
      }
    };
  }

  /* Json serialization */

  public static<T> JsonBinding<Error<T>> jsonBinding(JsonBinding<T> bindingT) {
    final JsonBinding<T> value = bindingT;
    final JsonBinding<String> error = JsonBindings.STRING;
    final Factory<T> factoryT = bindingT.factory();
    final Factory<Error<T>> _factory = factory(bindingT.factory());

    return new JsonBinding<Error<T>>() {
      public Factory<Error<T>> factory() {
        return _factory;
      }

      public JsonElement toJson(Error<T> _value) {
        JsonObject _result = new JsonObject();
        switch (_value.getDisc()) {
          case VALUE:
            _result.add("value", value.toJson(_value.getValue()));
            break;
          case ERROR:
            _result.add("error", error.toJson(_value.getError()));
            break;
        }
        return _result;
      }

      public Error<T> fromJson(JsonElement _json) {
        JsonObject _obj = _json.getAsJsonObject();
        for (Map.Entry<String,JsonElement> _v : _obj.entrySet()) {
          if (_v.getKey().equals("value")) {
            return Error.<T>value(value.fromJson(_v.getValue()));
          }
          else if (_v.getKey().equals("error")) {
            return Error.<T>error(error.fromJson(_v.getValue()));
          }
        }
        throw new IllegalStateException();
      }
    };
  }
}
