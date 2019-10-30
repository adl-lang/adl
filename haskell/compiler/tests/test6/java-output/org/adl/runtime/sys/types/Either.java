/* @generated from adl module sys.types */

package org.adl.runtime.sys.types;

import com.google.gson.JsonElement;
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
    Either<?, ?> other = (Either<?, ?>) other0;
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
      final Lazy<Factory<T1>> left = new Lazy<>(() -> factoryT1);
      final Lazy<Factory<T2>> right = new Lazy<>(() -> factoryT2);

      @Override
      public Either<T1, T2> create() {
        return new Either<T1, T2>(Disc.LEFT,left.get().create());
      }

      @Override
      public Either<T1, T2> create(Either<T1, T2> other) {
        switch (other.disc) {
          case LEFT:
            return new Either<T1, T2>(other.disc,left.get().create(Either.<T1>cast(other.value)));
          case RIGHT:
            return new Either<T1, T2>(other.disc,right.get().create(Either.<T2>cast(other.value)));
        }
        throw new IllegalArgumentException();
      }

      @Override
      public TypeExpr typeExpr() {
        ScopedName scopedName = new ScopedName("sys.types", "Either");
        ArrayList<TypeExpr> params = new ArrayList<>();
        params.add(factoryT1.typeExpr());
        params.add(factoryT2.typeExpr());
        return new TypeExpr(TypeRef.reference(scopedName), params);
      }

      @Override
      public JsonBinding<Either<T1, T2>> jsonBinding() {
        return Either.jsonBinding(factoryT1.jsonBinding(), factoryT2.jsonBinding());
      }
    };
  }

  /* Json serialization */

  public static<T1, T2> JsonBinding<Either<T1, T2>> jsonBinding(JsonBinding<T1> bindingT1, JsonBinding<T2> bindingT2) {
    final Lazy<JsonBinding<T1>> left = new Lazy<>(() -> bindingT1);
    final Lazy<JsonBinding<T2>> right = new Lazy<>(() -> bindingT2);
    final Factory<T1> factoryT1 = bindingT1.factory();
    final Factory<T2> factoryT2 = bindingT2.factory();
    final Factory<Either<T1, T2>> _factory = factory(bindingT1.factory(), bindingT2.factory());

    return new JsonBinding<Either<T1, T2>>() {
      @Override
      public Factory<Either<T1, T2>> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(Either<T1, T2> _value) {
        switch (_value.getDisc()) {
          case LEFT:
            return JsonBindings.unionToJson("left", _value.getLeft(), left.get());
          case RIGHT:
            return JsonBindings.unionToJson("right", _value.getRight(), right.get());
        }
        return null;
      }

      @Override
      public Either<T1, T2> fromJson(JsonElement _json) {
        String _key = JsonBindings.unionNameFromJson(_json);
        if (_key.equals("left")) {
          return Either.<T1, T2>left(JsonBindings.unionValueFromJson(_json, left.get()));
        }
        else if (_key.equals("right")) {
          return Either.<T1, T2>right(JsonBindings.unionValueFromJson(_json, right.get()));
        }
        throw new JsonParseException("Invalid discriminator " + _key + " for union Either<T1, T2>");
      }
    };
  }
}
