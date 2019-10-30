/* @generated from adl module sys.adlast */

package org.adl.runtime.sys.adlast;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.adl.runtime.Builders;
import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
import org.adl.runtime.Lazy;
import java.util.ArrayList;
import java.util.Objects;

public class TypeDef {

  /* Members */

  private ArrayList<String> typeParams;
  private TypeExpr typeExpr;

  /* Constructors */

  public TypeDef(ArrayList<String> typeParams, TypeExpr typeExpr) {
    this.typeParams = Objects.requireNonNull(typeParams);
    this.typeExpr = Objects.requireNonNull(typeExpr);
  }

  public TypeDef() {
    this.typeParams = new ArrayList<String>();
    this.typeExpr = new TypeExpr();
  }

  public TypeDef(TypeDef other) {
    this.typeParams = Factories.arrayList(Factories.STRING).create(other.typeParams);
    this.typeExpr = TypeExpr.FACTORY.create(other.typeExpr);
  }

  /* Accessors and mutators */

  public ArrayList<String> getTypeParams() {
    return typeParams;
  }

  public void setTypeParams(ArrayList<String> typeParams) {
    this.typeParams = Objects.requireNonNull(typeParams);
  }

  public TypeExpr getTypeExpr() {
    return typeExpr;
  }

  public void setTypeExpr(TypeExpr typeExpr) {
    this.typeExpr = Objects.requireNonNull(typeExpr);
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof TypeDef)) {
      return false;
    }
    TypeDef other = (TypeDef) other0;
    return
      typeParams.equals(other.typeParams) &&
      typeExpr.equals(other.typeExpr);
  }

  @Override
  public int hashCode() {
    int _result = 1;
    _result = _result * 37 + typeParams.hashCode();
    _result = _result * 37 + typeExpr.hashCode();
    return _result;
  }

  /* Builder */

  public static class Builder {
    private ArrayList<String> typeParams;
    private TypeExpr typeExpr;

    public Builder() {
      this.typeParams = null;
      this.typeExpr = null;
    }

    public Builder setTypeParams(ArrayList<String> typeParams) {
      this.typeParams = Objects.requireNonNull(typeParams);
      return this;
    }

    public Builder setTypeExpr(TypeExpr typeExpr) {
      this.typeExpr = Objects.requireNonNull(typeExpr);
      return this;
    }

    public TypeDef create() {
      Builders.checkFieldInitialized("TypeDef", "typeParams", typeParams);
      Builders.checkFieldInitialized("TypeDef", "typeExpr", typeExpr);
      return new TypeDef(typeParams, typeExpr);
    }
  }

  /* Factory for construction of generic values */

  public static final Factory<TypeDef> FACTORY = new Factory<TypeDef>() {
    @Override
    public TypeDef create() {
      return new TypeDef();
    }

    @Override
    public TypeDef create(TypeDef other) {
      return new TypeDef(other);
    }

    @Override
    public TypeExpr typeExpr() {
      ScopedName scopedName = new ScopedName("sys.adlast", "TypeDef");
      ArrayList<TypeExpr> params = new ArrayList<>();
      return new TypeExpr(TypeRef.reference(scopedName), params);
    }
    @Override
    public JsonBinding<TypeDef> jsonBinding() {
      return TypeDef.jsonBinding();
    }
  };

  /* Json serialization */

  public static JsonBinding<TypeDef> jsonBinding() {
    final Lazy<JsonBinding<ArrayList<String>>> typeParams = new Lazy<>(() -> JsonBindings.arrayList(JsonBindings.STRING));
    final Lazy<JsonBinding<TypeExpr>> typeExpr = new Lazy<>(() -> TypeExpr.jsonBinding());
    final Factory<TypeDef> _factory = FACTORY;

    return new JsonBinding<TypeDef>() {
      @Override
      public Factory<TypeDef> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(TypeDef _value) {
        JsonObject _result = new JsonObject();
        _result.add("typeParams", typeParams.get().toJson(_value.typeParams));
        _result.add("typeExpr", typeExpr.get().toJson(_value.typeExpr));
        return _result;
      }

      @Override
      public TypeDef fromJson(JsonElement _json) {
        JsonObject _obj = JsonBindings.objectFromJson(_json);
        return new TypeDef(
          JsonBindings.fieldFromJson(_obj, "typeParams", typeParams.get()),
          JsonBindings.fieldFromJson(_obj, "typeExpr", typeExpr.get())
        );
      }
    };
  }
}
