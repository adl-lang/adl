package org.adl.sys.types;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
import java.util.Map;
import java.util.Objects;

public class Maybe<T> {

  /* Members */

  private Disc disc;
  private Object value;

  /**
   * The Maybe discriminator type.
   */
  public enum Disc {
    NOTHING,
    JUST
  }

  /* Constructors */

  public static <T> Maybe<T> nothing() {
    return new Maybe<T>(Disc.NOTHING, null);
  }

  public static <T> Maybe<T> just(T v) {
    return new Maybe<T>(Disc.JUST, Objects.requireNonNull(v));
  }

  private Maybe(Disc disc, Object value) {
    this.disc = disc;
    this.value = value;
  }

  /* Accessors */

  public Disc getDisc() {
    return disc;
  }

  public T getJust() {
    if (disc == Disc.JUST) {
      return Maybe.<T>cast(value);
    }
    throw new IllegalStateException();
  }

  /* Mutators */

  public void setNothing() {
    this.value = null;
    this.disc = Disc.NOTHING;
  }

  public void setJust(T v) {
    this.value = Objects.requireNonNull(v);
    this.disc = Disc.JUST;
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof Maybe)) {
      return false;
    }
    Maybe other = (Maybe) other0;
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

  public static <T> Factory<Maybe <T>> factory(Factory<T> factoryT) {
    return new Factory<Maybe<T>>() {
      final Factory<Void> nothing = Factories.VOID;
      final Factory<T> just = factoryT;

      public Maybe<T> create() {
        return new Maybe<T>(Disc.NOTHING,nothing.create());
      }

      public Maybe<T> create(Maybe<T> other) {
        switch (other.disc) {
          case NOTHING:
            return new Maybe<T>(other.disc,other.value);
          case JUST:
            return new Maybe<T>(other.disc,just.create(Maybe.<T>cast(other.value)));
        }
        throw new IllegalArgumentException();
      }
    };
  }

  /* Json serialization */

  public static<T> JsonBinding<Maybe<T>> jsonBinding(JsonBinding<T> bindingT) {
    final JsonBinding<Void> nothing = JsonBindings.VOID;
    final JsonBinding<T> just = bindingT;
    final Factory<T> factoryT = bindingT.factory();
    final Factory<Maybe<T>> _factory = factory(bindingT.factory());

    return new JsonBinding<Maybe<T>>() {
      public Factory<Maybe<T>> factory() {
        return _factory;
      }

      public JsonElement toJson(Maybe<T> _value) {
        JsonObject _result = new JsonObject();
        switch (_value.getDisc()) {
          case NOTHING:
            _result.add("nothing", null);
          case JUST:
            _result.add("just", just.toJson(_value.getJust()));
            break;
        }
        return _result;
      }

      public Maybe<T> fromJson(JsonElement _json) {
        JsonObject _obj = _json.getAsJsonObject();
        for (Map.Entry<String,JsonElement> _v : _obj.entrySet()) {
          if (_v.getKey() == "nothing") {
            return Maybe.<T>nothing();
          }
          else if (_v.getKey() == "just") {
            return Maybe.<T>just(just.fromJson(_v.getValue()));
          }
        }
        throw new IllegalStateException();
      }
    };
  }
}
