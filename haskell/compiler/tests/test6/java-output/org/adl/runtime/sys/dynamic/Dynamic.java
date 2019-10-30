/* @generated from adl module sys.dynamic */

package org.adl.runtime.sys.dynamic;

import com.google.gson.JsonElement;
import com.google.gson.JsonNull;
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

/**
 * A serialised value along with  its type
 */
public class Dynamic {

  /* Members */

  private TypeExpr typeExpr;
  private JsonElement value;

  /* Constructors */

  public Dynamic(TypeExpr typeExpr, JsonElement value) {
    this.typeExpr = Objects.requireNonNull(typeExpr);
    this.value = Objects.requireNonNull(value);
  }

  public Dynamic() {
    this.typeExpr = new TypeExpr();
    this.value = JsonNull.INSTANCE;
  }

  public Dynamic(Dynamic other) {
    this.typeExpr = TypeExpr.FACTORY.create(other.typeExpr);
    this.value = other.value;
  }

  /* Accessors and mutators */

  public TypeExpr getTypeExpr() {
    return typeExpr;
  }

  public void setTypeExpr(TypeExpr typeExpr) {
    this.typeExpr = Objects.requireNonNull(typeExpr);
  }

  public JsonElement getValue() {
    return value;
  }

  public void setValue(JsonElement value) {
    this.value = Objects.requireNonNull(value);
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof Dynamic)) {
      return false;
    }
    Dynamic other = (Dynamic) other0;
    return
      typeExpr.equals(other.typeExpr) &&
      value.equals(other.value);
  }

  @Override
  public int hashCode() {
    int _result = 1;
    _result = _result * 37 + typeExpr.hashCode();
    _result = _result * 37 + value.hashCode();
    return _result;
  }

  /* Builder */

  public static class Builder {
    private TypeExpr typeExpr;
    private JsonElement value;

    public Builder() {
      this.typeExpr = null;
      this.value = null;
    }

    public Builder setTypeExpr(TypeExpr typeExpr) {
      this.typeExpr = Objects.requireNonNull(typeExpr);
      return this;
    }

    public Builder setValue(JsonElement value) {
      this.value = Objects.requireNonNull(value);
      return this;
    }

    public Dynamic create() {
      Builders.checkFieldInitialized("Dynamic", "typeExpr", typeExpr);
      Builders.checkFieldInitialized("Dynamic", "value", value);
      return new Dynamic(typeExpr, value);
    }
  }

  /* Factory for construction of generic values */

  public static final Factory<Dynamic> FACTORY = new Factory<Dynamic>() {
    @Override
    public Dynamic create() {
      return new Dynamic();
    }

    @Override
    public Dynamic create(Dynamic other) {
      return new Dynamic(other);
    }

    @Override
    public TypeExpr typeExpr() {
      ScopedName scopedName = new ScopedName("sys.dynamic", "Dynamic");
      ArrayList<TypeExpr> params = new ArrayList<>();
      return new TypeExpr(TypeRef.reference(scopedName), params);
    }
    @Override
    public JsonBinding<Dynamic> jsonBinding() {
      return Dynamic.jsonBinding();
    }
  };

  /* Json serialization */

  public static JsonBinding<Dynamic> jsonBinding() {
    final Lazy<JsonBinding<TypeExpr>> typeExpr = new Lazy<>(() -> TypeExpr.jsonBinding());
    final Lazy<JsonBinding<JsonElement>> value = new Lazy<>(() -> JsonBindings.JSON);
    final Factory<Dynamic> _factory = FACTORY;

    return new JsonBinding<Dynamic>() {
      @Override
      public Factory<Dynamic> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(Dynamic _value) {
        JsonObject _result = new JsonObject();
        _result.add("typeExpr", typeExpr.get().toJson(_value.typeExpr));
        _result.add("value", value.get().toJson(_value.value));
        return _result;
      }

      @Override
      public Dynamic fromJson(JsonElement _json) {
        JsonObject _obj = JsonBindings.objectFromJson(_json);
        return new Dynamic(
          JsonBindings.fieldFromJson(_obj, "typeExpr", typeExpr.get()),
          JsonBindings.fieldFromJson(_obj, "value", value.get())
        );
      }
    };
  }
}
