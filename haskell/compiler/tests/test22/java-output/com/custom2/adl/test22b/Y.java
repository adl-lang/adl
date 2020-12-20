/* @generated from adl module test22b */

package com.custom2.adl.test22b;

import com.custom.adl.test22a.X;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
import org.adl.runtime.Lazy;
import org.adl.runtime.sys.adlast.ScopedName;
import org.adl.runtime.sys.adlast.TypeExpr;
import org.adl.runtime.sys.adlast.TypeRef;
import java.util.ArrayList;
import java.util.Objects;

public class Y {

  /* Members */

  private X field;

  /* Constructors */

  public Y(X field) {
    this.field = Objects.requireNonNull(field);
  }

  public Y(Y other) {
    this.field = X.FACTORY.create(other.field);
  }

  /* Accessors and mutators */

  public X getField() {
    return field;
  }

  public Y setField(X field) {
    this.field = Objects.requireNonNull(field);
    return this;
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof Y)) {
      return false;
    }
    Y other = (Y) other0;
    return
      field.equals(other.field);
  }

  @Override
  public int hashCode() {
    int _result = 1;
    _result = _result * 37 + field.hashCode();
    return _result;
  }

  /* Factory for construction of generic values */

  public static final Factory<Y> FACTORY = new Factory<Y>() {
    @Override
    public Y create(Y other) {
      return new Y(other);
    }

    @Override
    public TypeExpr typeExpr() {
      ScopedName scopedName = new ScopedName("test22b", "Y");
      ArrayList<TypeExpr> params = new ArrayList<>();
      return new TypeExpr(TypeRef.reference(scopedName), params);
    }
    @Override
    public JsonBinding<Y> jsonBinding() {
      return Y.jsonBinding();
    }
  };

  /* Json serialization */

  public static JsonBinding<Y> jsonBinding() {
    final Lazy<JsonBinding<X>> field = new Lazy<>(() -> X.jsonBinding());
    final Factory<Y> _factory = FACTORY;

    return new JsonBinding<Y>() {
      @Override
      public Factory<Y> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(Y _value) {
        JsonObject _result = new JsonObject();
        _result.add("field", field.get().toJson(_value.field));
        return _result;
      }

      @Override
      public Y fromJson(JsonElement _json) {
        JsonObject _obj = JsonBindings.objectFromJson(_json);
        return new Y(
          JsonBindings.fieldFromJson(_obj, "field", field.get())
        );
      }
    };
  }
}
