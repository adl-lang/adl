package org.adl.sys.types;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import java.util.Map;
import java.util.Objects;

public class Either<T1, T2> {

  /* Members */

  private Disc disc;
  private Object value;

  /**
   * The Either discriminator type.
   */
  public enum Disc {
    LEFT,
    RIGHT
  }

  /* Constructors */

  public static <T1, T2> Either<T1, T2> left(T1 v) {
    return new Either<T1, T2>(Disc.LEFT, Objects.requireNonNull(v));
  }

  public static <T1, T2> Either<T1, T2> right(T2 v) {
    return new Either<T1, T2>(Disc.RIGHT, Objects.requireNonNull(v));
  }

  private Either(Disc disc, Object value) {
    this.disc = disc;
    this.value = value;
  }

  /* Accessors */

  public Disc getDisc() {
    return disc;
  }

  public T1 getLeft() {
    if (disc == Disc.LEFT) {
      return Either.<T1>cast(value);
    }
    throw new IllegalStateException();
  }

  public T2 getRight() {
    if (disc == Disc.RIGHT) {
      return Either.<T2>cast(value);
    }
    throw new IllegalStateException();
  }

  /* Mutators */

  public void setLeft(T1 v) {
    this.value = Objects.requireNonNull(v);
    this.disc = Disc.LEFT;
  }

  public void setRight(T2 v) {
    this.value = Objects.requireNonNull(v);
    this.disc = Disc.RIGHT;
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof Either)) {
      return false;
    }
    Either other = (Either) other0;
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

  public static <T1, T2> Factory<Either <T1, T2>> factory(Factory<T1> factoryT1, Factory<T2> factoryT2) {
    return new Factory<Either<T1, T2>>() {
      final Factory<T1> left = factoryT1;
      final Factory<T2> right = factoryT2;

      public Either<T1, T2> create() {
        return new Either<T1, T2>(Disc.LEFT,left.create());
      }

      public Either<T1, T2> create(Either<T1, T2> other) {
        switch (other.disc) {
          case LEFT:
            return new Either<T1, T2>(other.disc,left.create(Either.<T1>cast(other.value)));
          case RIGHT:
            return new Either<T1, T2>(other.disc,right.create(Either.<T2>cast(other.value)));
        }
        throw new IllegalArgumentException();
      }
    };
  }

  /* Json serialization */

  public static<T1, T2> JsonBinding<Either<T1, T2>> jsonBinding(JsonBinding<T1> bindingT1, JsonBinding<T2> bindingT2) {
    final JsonBinding<T1> left = bindingT1;
    final JsonBinding<T2> right = bindingT2;
    final Factory<T1> factoryT1 = bindingT1.factory();
    final Factory<T2> factoryT2 = bindingT2.factory();
    final Factory<Either<T1, T2>> _factory = factory(bindingT1.factory(), bindingT2.factory());

    return new JsonBinding<Either<T1, T2>>() {
      public Factory<Either<T1, T2>> factory() {
        return _factory;
      }

      public JsonElement toJson(Either<T1, T2> _value) {
        JsonObject _result = new JsonObject();
        switch (_value.getDisc()) {
          case LEFT:
            _result.add("left", left.toJson(_value.getLeft()));
            break;
          case RIGHT:
            _result.add("right", right.toJson(_value.getRight()));
            break;
        }
        return _result;
      }

      public Either<T1, T2> fromJson(JsonElement _json) {
        JsonObject _obj = _json.getAsJsonObject();
        for (Map.Entry<String,JsonElement> _v : _obj.entrySet()) {
          if (_v.getKey() == "left") {
            return Either.<T1, T2>left(left.fromJson(_v.getValue()));
          }
          else if (_v.getKey() == "right") {
            return Either.<T1, T2>right(right.fromJson(_v.getValue()));
          }
        }
        throw new IllegalStateException();
      }
    };
  }
}
