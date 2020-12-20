/* @generated from adl module test22a */

package com.custom.adl.test22a;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
import org.adl.runtime.Lazy;
import org.adl.runtime.sys.adlast.ScopedName;
import org.adl.runtime.sys.adlast.TypeExpr;
import org.adl.runtime.sys.adlast.TypeRef;
import java.util.ArrayList;
import java.util.Objects;

public class X {

  /* Members */

  private String field;

  /* Constructors */

  public X(String field) {
    this.field = Objects.requireNonNull(field);
  }

  public X(X other) {
    this.field = other.field;
  }

  /* Accessors and mutators */

  public String getField() {
    return field;
  }

  public X setField(String field) {
    this.field = Objects.requireNonNull(field);
    return this;
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof X)) {
      return false;
    }
    X other = (X) other0;
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

  public static final Factory<X> FACTORY = new Factory<X>() {
    @Override
    public X create(X other) {
      return new X(other);
    }

    @Override
    public TypeExpr typeExpr() {
      ScopedName scopedName = new ScopedName("test22a", "X");
      ArrayList<TypeExpr> params = new ArrayList<>();
      return new TypeExpr(TypeRef.reference(scopedName), params);
    }
    @Override
    public JsonBinding<X> jsonBinding() {
      return X.jsonBinding();
    }
  };

  /* Json serialization */

  public static JsonBinding<X> jsonBinding() {
    final Lazy<JsonBinding<String>> field = new Lazy<>(() -> JsonBindings.STRING);
    final Factory<X> _factory = FACTORY;

    return new JsonBinding<X>() {
      @Override
      public Factory<X> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(X _value) {
        JsonObject _result = new JsonObject();
        _result.add("field", field.get().toJson(_value.field));
        return _result;
      }

      @Override
      public X fromJson(JsonElement _json) {
        JsonObject _obj = JsonBindings.objectFromJson(_json);
        return new X(
          JsonBindings.fieldFromJson(_obj, "field", field.get())
        );
      }
    };
  }
}
