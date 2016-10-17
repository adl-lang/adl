package org.adl.sys.adlast;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
import org.adl.runtime.Lazy;
import org.adl.runtime.OptionalHelpers;
import java.util.Objects;
import java.util.Optional;

public class Field {

  /* Members */

  private String name;
  private TypeExpr typeExpr;
  private Optional<Literal> default_;

  /* Constructors */

  public Field(String name, TypeExpr typeExpr, Optional<Literal> default_) {
    this.name = Objects.requireNonNull(name);
    this.typeExpr = Objects.requireNonNull(typeExpr);
    this.default_ = Objects.requireNonNull(default_);
  }

  public Field() {
    this.name = "";
    this.typeExpr = new TypeExpr();
    this.default_ = OptionalHelpers.factory(Literal.FACTORY).create();
  }

  public Field(Field other) {
    this.name = other.name;
    this.typeExpr = TypeExpr.FACTORY.create(other.typeExpr);
    this.default_ = OptionalHelpers.factory(Literal.FACTORY).create(other.default_);
  }

  /* Accessors and mutators */

  public String getName() {
    return name;
  }

  public void setName(String name) {
    this.name = Objects.requireNonNull(name);
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
    if (!(other0 instanceof Field)) {
      return false;
    }
    Field other = (Field) other0;
    return
      name.equals(other.name) &&
      typeExpr.equals(other.typeExpr) &&
      default_.equals(other.default_);
  }

  @Override
  public int hashCode() {
    int result = 1;
    result = result * 37 + name.hashCode();
    result = result * 37 + typeExpr.hashCode();
    result = result * 37 + default_.hashCode();
    return result;
  }

  /* Factory for construction of generic values */

  public static final Factory<Field> FACTORY = new Factory<Field>() {
    public Field create() {
      return new Field();
    }
    public Field create(Field other) {
      return new Field(other);
    }
  };

  /* Json serialization */

  public static JsonBinding<Field> jsonBinding() {
    final Lazy<JsonBinding<String>> name = new Lazy<>(() -> JsonBindings.STRING);
    final Lazy<JsonBinding<TypeExpr>> typeExpr = new Lazy<>(() -> TypeExpr.jsonBinding());
    final Lazy<JsonBinding<Optional<Literal>>> default_ = new Lazy<>(() -> OptionalHelpers.jsonBinding(Literal.jsonBinding()));
    final Factory<Field> _factory = FACTORY;

    return new JsonBinding<Field>() {
      public Factory<Field> factory() {
        return _factory;
      }

      public JsonElement toJson(Field _value) {
        JsonObject _result = new JsonObject();
        _result.add("name", name.get().toJson(_value.name));
        _result.add("typeExpr", typeExpr.get().toJson(_value.typeExpr));
        _result.add("default", default_.get().toJson(_value.default_));
        return _result;
      }

      public Field fromJson(JsonElement _json) {
        JsonObject _obj = _json.getAsJsonObject();
        return new Field(
          _obj.has("name") ? name.get().fromJson(_obj.get("name")) : "",
          _obj.has("typeExpr") ? typeExpr.get().fromJson(_obj.get("typeExpr")) : new TypeExpr(),
          _obj.has("default") ? default_.get().fromJson(_obj.get("default")) : OptionalHelpers.factory(Literal.FACTORY).create()
        );
      }
    };
  }
}
