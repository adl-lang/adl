package org.adl.sys.types;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonPrimitive;
import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
import org.adl.runtime.Lazy;
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
    switch (disc) {
      case NOTHING:
        return disc == other.disc;
      case JUST:
        return disc == other.disc && value.equals(other.value);
    }
    throw new IllegalStateException();
  }

  @Override
  public int hashCode() {
    switch (disc) {
      case NOTHING:
        return disc.hashCode();
      case JUST:
        return disc.hashCode() * 37 + value.hashCode();
    }
    throw new IllegalStateException();
  }

  @SuppressWarnings("unchecked")
  private static <T> T cast(final Object o) {
    return (T) o;
  }

  /* Factory for construction of generic values */

  public static <T> Factory<Maybe <T>> factory(Factory<T> factoryT) {
    return new Factory<Maybe<T>>() {
      final Lazy<Factory<Void>> nothing = new Lazy<>(() -> Factories.VOID);
      final Lazy<Factory<T>> just = new Lazy<>(() -> factoryT);

      public Maybe<T> create() {
        return new Maybe<T>(Disc.NOTHING,nothing.get().create());
      }

      public Maybe<T> create(Maybe<T> other) {
        switch (other.disc) {
          case NOTHING:
            return new Maybe<T>(other.disc,other.value);
          case JUST:
            return new Maybe<T>(other.disc,just.get().create(Maybe.<T>cast(other.value)));
        }
        throw new IllegalArgumentException();
      }
    };
  }

  /* Json serialization */

  public static<T> JsonBinding<Maybe<T>> jsonBinding(JsonBinding<T> bindingT) {
    final Lazy<JsonBinding<Void>> nothing = new Lazy<>(() -> JsonBindings.VOID);
    final Lazy<JsonBinding<T>> just = new Lazy<>(() -> bindingT);
    final Factory<T> factoryT = bindingT.factory();
    final Factory<Maybe<T>> _factory = factory(bindingT.factory());

    return new JsonBinding<Maybe<T>>() {
      public Factory<Maybe<T>> factory() {
        return _factory;
      }

      public JsonElement toJson(Maybe<T> _value) {
        switch (_value.getDisc()) {
          case NOTHING:
            return JsonBindings.unionToJson("nothing", null, null);
          case JUST:
            return JsonBindings.unionToJson("just", _value.getJust(), just.get());
        }
        return null;
      }

      public Maybe<T> fromJson(JsonElement _json) {
        String _key = JsonBindings.unionNameFromJson(_json);
        if (_key.equals("nothing")) {
          return Maybe.<T>nothing();
        }
        else if (_key.equals("just")) {
          return Maybe.<T>just(JsonBindings.unionValueFromJson(_json, just.get()));
        }
        throw new IllegalStateException();
      }
    };
  }
}
