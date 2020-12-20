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

public class Result<T, E> {

  /* Members */

  private Disc disc;
  private Object value;

  /**
   * The Result discriminator type.
   */
  public enum Disc {
    OK,
    ERROR
  }

  /* Constructors */

  public static <T, E> Result<T, E> ok(T v) {
    return new Result<T, E>(Disc.OK, Objects.requireNonNull(v));
  }

  public static <T, E> Result<T, E> error(E v) {
    return new Result<T, E>(Disc.ERROR, Objects.requireNonNull(v));
  }

  private Result(Disc disc, Object value) {
    this.disc = disc;
    this.value = value;
  }

  /* Accessors */

  public Disc getDisc() {
    return disc;
  }

  public T getOk() {
    if (disc == Disc.OK) {
      return Result.<T>cast(value);
    }
    throw new IllegalStateException();
  }

  public E getError() {
    if (disc == Disc.ERROR) {
      return Result.<E>cast(value);
    }
    throw new IllegalStateException();
  }

  /* Mutators */

  public void setOk(T v) {
    this.value = Objects.requireNonNull(v);
    this.disc = Disc.OK;
  }

  public void setError(E v) {
    this.value = Objects.requireNonNull(v);
    this.disc = Disc.ERROR;
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof Result)) {
      return false;
    }
    Result<?, ?> other = (Result<?, ?>) other0;
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

  public static <T, E> Factory<Result <T, E>> factory(Factory<T> factoryT, Factory<E> factoryE) {
    return new Factory<Result<T, E>>() {
      final Lazy<Factory<T>> ok = new Lazy<>(() -> factoryT);
      final Lazy<Factory<E>> error = new Lazy<>(() -> factoryE);


      @Override
      public Result<T, E> create(Result<T, E> other) {
        switch (other.disc) {
          case OK:
            return new Result<T, E>(other.disc,ok.get().create(Result.<T>cast(other.value)));
          case ERROR:
            return new Result<T, E>(other.disc,error.get().create(Result.<E>cast(other.value)));
        }
        throw new IllegalArgumentException();
      }

      @Override
      public TypeExpr typeExpr() {
        ScopedName scopedName = new ScopedName("sys.types", "Result");
        ArrayList<TypeExpr> params = new ArrayList<>();
        params.add(factoryT.typeExpr());
        params.add(factoryE.typeExpr());
        return new TypeExpr(TypeRef.reference(scopedName), params);
      }

      @Override
      public JsonBinding<Result<T, E>> jsonBinding() {
        return Result.jsonBinding(factoryT.jsonBinding(), factoryE.jsonBinding());
      }
    };
  }

  /* Json serialization */

  public static<T, E> JsonBinding<Result<T, E>> jsonBinding(JsonBinding<T> bindingT, JsonBinding<E> bindingE) {
    final Lazy<JsonBinding<T>> ok = new Lazy<>(() -> bindingT);
    final Lazy<JsonBinding<E>> error = new Lazy<>(() -> bindingE);
    final Factory<T> factoryT = bindingT.factory();
    final Factory<E> factoryE = bindingE.factory();
    final Factory<Result<T, E>> _factory = factory(bindingT.factory(), bindingE.factory());

    return new JsonBinding<Result<T, E>>() {
      @Override
      public Factory<Result<T, E>> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(Result<T, E> _value) {
        switch (_value.getDisc()) {
          case OK:
            return JsonBindings.unionToJson("ok", _value.getOk(), ok.get());
          case ERROR:
            return JsonBindings.unionToJson("error", _value.getError(), error.get());
        }
        return null;
      }

      @Override
      public Result<T, E> fromJson(JsonElement _json) {
        String _key = JsonBindings.unionNameFromJson(_json);
        if (_key.equals("ok")) {
          return Result.<T, E>ok(JsonBindings.unionValueFromJson(_json, ok.get()));
        }
        else if (_key.equals("error")) {
          return Result.<T, E>error(JsonBindings.unionValueFromJson(_json, error.get()));
        }
        throw new JsonParseException("Invalid discriminator " + _key + " for union Result<T, E>");
      }
    };
  }
}
