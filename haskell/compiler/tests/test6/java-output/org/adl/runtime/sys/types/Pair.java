/* @generated from adl module sys.types */

package org.adl.runtime.sys.types;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.adl.runtime.Builders;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
import org.adl.runtime.Lazy;
import org.adl.runtime.sys.adlast.ScopedName;
import org.adl.runtime.sys.adlast.TypeExpr;
import org.adl.runtime.sys.adlast.TypeRef;
import java.util.ArrayList;
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

  public void setV1(T1 v1) {
    this.v1 = Objects.requireNonNull(v1);
  }

  public T2 getV2() {
    return v2;
  }

  public void setV2(T2 v2) {
    this.v2 = Objects.requireNonNull(v2);
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof Pair)) {
      return false;
    }
    Pair<?, ?> other = (Pair<?, ?>) other0;
    return
      v1.equals(other.v1) &&
      v2.equals(other.v2);
  }

  @Override
  public int hashCode() {
    int _result = 1;
    _result = _result * 37 + v1.hashCode();
    _result = _result * 37 + v2.hashCode();
    return _result;
  }

  /* Builder */

  public static class Builder<T1, T2> {
    private T1 v1;
    private T2 v2;

    public Builder() {
      this.v1 = null;
      this.v2 = null;
    }

    public Builder<T1, T2> setV1(T1 v1) {
      this.v1 = Objects.requireNonNull(v1);
      return this;
    }

    public Builder<T1, T2> setV2(T2 v2) {
      this.v2 = Objects.requireNonNull(v2);
      return this;
    }

    public Pair<T1, T2> create() {
      Builders.checkFieldInitialized("Pair", "v1", v1);
      Builders.checkFieldInitialized("Pair", "v2", v2);
      return new Pair<T1, T2>(v1, v2);
    }
  }

  /* Factory for construction of generic values */

  public static <T1, T2> Factory<Pair<T1, T2>> factory(Factory<T1> factoryT1, Factory<T2> factoryT2) {
    return new Factory<Pair<T1, T2>>() {
      final Lazy<Factory<T1>> v1 = new Lazy<>(() -> factoryT1);
      final Lazy<Factory<T2>> v2 = new Lazy<>(() -> factoryT2);

      @Override
      public Pair<T1, T2> create() {
        return new Pair<T1, T2>(
          v1.get().create(),
          v2.get().create()
          );
      }

      @Override
      public Pair<T1, T2> create(Pair<T1, T2> other) {
        return new Pair<T1, T2>(
          v1.get().create(other.getV1()),
          v2.get().create(other.getV2())
          );
      }

      @Override
      public TypeExpr typeExpr() {
        ScopedName scopedName = new ScopedName("sys.types", "Pair");
        ArrayList<TypeExpr> params = new ArrayList<>();
        params.add(factoryT1.typeExpr());
        params.add(factoryT2.typeExpr());
        return new TypeExpr(TypeRef.reference(scopedName), params);
      }

      @Override
      public JsonBinding<Pair<T1, T2>> jsonBinding() {
        return Pair.jsonBinding(factoryT1.jsonBinding(), factoryT2.jsonBinding());
      }
    };
  }

  /* Json serialization */

  public static<T1, T2> JsonBinding<Pair<T1, T2>> jsonBinding(JsonBinding<T1> bindingT1, JsonBinding<T2> bindingT2) {
    final Lazy<JsonBinding<T1>> v1 = new Lazy<>(() -> bindingT1);
    final Lazy<JsonBinding<T2>> v2 = new Lazy<>(() -> bindingT2);
    final Factory<T1> factoryT1 = bindingT1.factory();
    final Factory<T2> factoryT2 = bindingT2.factory();
    final Factory<Pair<T1, T2>> _factory = factory(bindingT1.factory(), bindingT2.factory());

    return new JsonBinding<Pair<T1, T2>>() {
      @Override
      public Factory<Pair<T1, T2>> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(Pair<T1, T2> _value) {
        JsonObject _result = new JsonObject();
        _result.add("v1", v1.get().toJson(_value.v1));
        _result.add("v2", v2.get().toJson(_value.v2));
        return _result;
      }

      @Override
      public Pair<T1, T2> fromJson(JsonElement _json) {
        JsonObject _obj = JsonBindings.objectFromJson(_json);
        return new Pair<T1, T2>(
          JsonBindings.fieldFromJson(_obj, "v1", v1.get()),
          JsonBindings.fieldFromJson(_obj, "v2", v2.get())
        );
      }
    };
  }
}
