/* @generated from adl module sys.types */

package org.adl.runtime.sys.types;

import com.google.gson.JsonElement;
import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
import org.adl.runtime.JsonParseException;
import org.adl.runtime.Lazy;
import org.adl.runtime.sys.adlast.ScopedName;
import org.adl.runtime.sys.adlast.TypeExpr;
import org.adl.runtime.sys.adlast.TypeRef;
import java.util.ArrayList;
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
    Error<?> other = (Error<?>) other0;
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
      final Lazy<Factory<T>> value = new Lazy<>(() -> factoryT);
      final Lazy<Factory<String>> error = new Lazy<>(() -> Factories.STRING);

      @Override
      public Error<T> create() {
        return new Error<T>(Disc.VALUE,value.get().create());
      }

      @Override
      public Error<T> create(Error<T> other) {
        switch (other.disc) {
          case VALUE:
            return new Error<T>(other.disc,value.get().create(Error.<T>cast(other.value)));
          case ERROR:
            return new Error<T>(other.disc,other.value);
        }
        throw new IllegalArgumentException();
      }

      @Override
      public TypeExpr typeExpr() {
        ScopedName scopedName = new ScopedName("sys.types", "Error");
        ArrayList<TypeExpr> params = new ArrayList<>();
        params.add(factoryT.typeExpr());
        return new TypeExpr(TypeRef.reference(scopedName), params);
      }

      @Override
      public JsonBinding<Error<T>> jsonBinding() {
        return Error.jsonBinding(factoryT.jsonBinding());
      }
    };
  }

  /* Json serialization */

  public static<T> JsonBinding<Error<T>> jsonBinding(JsonBinding<T> bindingT) {
    final Lazy<JsonBinding<T>> value = new Lazy<>(() -> bindingT);
    final Lazy<JsonBinding<String>> error = new Lazy<>(() -> JsonBindings.STRING);
    final Factory<T> factoryT = bindingT.factory();
    final Factory<Error<T>> _factory = factory(bindingT.factory());

    return new JsonBinding<Error<T>>() {
      @Override
      public Factory<Error<T>> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(Error<T> _value) {
        switch (_value.getDisc()) {
          case VALUE:
            return JsonBindings.unionToJson("value", _value.getValue(), value.get());
          case ERROR:
            return JsonBindings.unionToJson("error", _value.getError(), error.get());
        }
        return null;
      }

      @Override
      public Error<T> fromJson(JsonElement _json) {
        String _key = JsonBindings.unionNameFromJson(_json);
        if (_key.equals("value")) {
          return Error.<T>value(JsonBindings.unionValueFromJson(_json, value.get()));
        }
        else if (_key.equals("error")) {
          return Error.<T>error(JsonBindings.unionValueFromJson(_json, error.get()));
        }
        throw new JsonParseException("Invalid discriminator " + _key + " for union Error<T>");
      }
    };
  }
}
