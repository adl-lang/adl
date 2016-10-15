package org.adl.sys.adlast;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
import org.adl.runtime.OptionalHelpers;
import java.util.ArrayList;
import java.util.Objects;
import java.util.Optional;

public class NewType {

  /* Members */

  private ArrayList<String> typeParams;
  private TypeExpr typeExpr;
  private Optional<Literal> default_;

  /* Constructors */

  public NewType(ArrayList<String> typeParams, TypeExpr typeExpr, Optional<Literal> default_) {
    this.typeParams = Objects.requireNonNull(typeParams);
    this.typeExpr = Objects.requireNonNull(typeExpr);
    this.default_ = Objects.requireNonNull(default_);
  }

  public NewType() {
    this.typeParams = new ArrayList<String>();
    this.typeExpr = new TypeExpr();
    this.default_ = OptionalHelpers.factory(Literal.FACTORY).create();
  }

  public NewType(NewType other) {
    this.typeParams = Factories.arrayList(Factories.STRING).create(other.typeParams);
    this.typeExpr = TypeExpr.FACTORY.create(other.typeExpr);
    this.default_ = OptionalHelpers.factory(Literal.FACTORY).create(other.default_);
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

  public Optional<Literal> getDefault() {
    return default_;
  }

  public void setDefault(Optional<Literal> default_) {
    this.default_ = Objects.requireNonNull(default_);
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof NewType)) {
      return false;
    }
    NewType other = (NewType) other0;
    return
      typeParams.equals(other.typeParams) &&
      typeExpr.equals(other.typeExpr) &&
      default_.equals(other.default_);
  }

  @Override
  public int hashCode() {
    int result = 1;
    result = result * 37 + typeParams.hashCode();
    result = result * 37 + typeExpr.hashCode();
    result = result * 37 + default_.hashCode();
    return result;
  }

  /* Factory for construction of generic values */

  public static final Factory<NewType> FACTORY = new Factory<NewType>() {
    public NewType create() {
      return new NewType();
    }
    public NewType create(NewType other) {
      return new NewType(other);
    }
  };

  /* Json serialization */

  public static JsonBinding<NewType> jsonBinding() {
    final JsonBinding<ArrayList<String>> typeParams = JsonBindings.arrayList(JsonBindings.STRING);
    final JsonBinding<TypeExpr> typeExpr = TypeExpr.jsonBinding();
    final JsonBinding<Optional<Literal>> default_ = OptionalHelpers.jsonBinding(Literal.jsonBinding());
    final Factory<NewType> _factory = FACTORY;

    return new JsonBinding<NewType>() {
      public Factory<NewType> factory() {
        return _factory;
      }

      public JsonElement toJson(NewType _value) {
        JsonObject _result = new JsonObject();
        _result.add("typeParams", typeParams.toJson(_value.typeParams));
        _result.add("typeExpr", typeExpr.toJson(_value.typeExpr));
        _result.add("default", default_.toJson(_value.default_));
        return _result;
      }

      public NewType fromJson(JsonElement _json) {
        JsonObject _obj = _json.getAsJsonObject();
        return new NewType(
          _obj.has("typeParams") ? typeParams.fromJson(_obj.get("typeParams")) : new ArrayList<String>(),
          _obj.has("typeExpr") ? typeExpr.fromJson(_obj.get("typeExpr")) : new TypeExpr(),
          _obj.has("default") ? default_.fromJson(_obj.get("default")) : OptionalHelpers.factory(Literal.FACTORY).create()
        );
      }
    };
  }
}
