package org.adl.sys.adlast;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import org.adl.runtime.HashMapHelpers;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
import org.adl.runtime.Lazy;
import org.adl.runtime.MaybeHelpers;
import java.util.HashMap;
import java.util.Objects;
import java.util.Optional;

public class Field {

  /* Members */

  private String name;
  private TypeExpr typeExpr;
  private Optional<Literal> default_;
  private HashMap<ScopedName, Literal> annotations;

  /* Constructors */

  public Field(String name, TypeExpr typeExpr, Optional<Literal> default_, HashMap<ScopedName, Literal> annotations) {
    this.name = Objects.requireNonNull(name);
    this.typeExpr = Objects.requireNonNull(typeExpr);
    this.default_ = Objects.requireNonNull(default_);
    this.annotations = Objects.requireNonNull(annotations);
  }

  public Field() {
    this.name = "";
    this.typeExpr = new TypeExpr();
    this.default_ = MaybeHelpers.factory(Literal.FACTORY).create();
    this.annotations = HashMapHelpers.factory(ScopedName.FACTORY, Literal.FACTORY).create();
  }

  public Field(Field other) {
    this.name = other.name;
    this.typeExpr = TypeExpr.FACTORY.create(other.typeExpr);
    this.default_ = MaybeHelpers.factory(Literal.FACTORY).create(other.default_);
    this.annotations = HashMapHelpers.factory(ScopedName.FACTORY, Literal.FACTORY).create(other.annotations);
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

  public HashMap<ScopedName, Literal> getAnnotations() {
    return annotations;
  }

  public void setAnnotations(HashMap<ScopedName, Literal> annotations) {
    this.annotations = Objects.requireNonNull(annotations);
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
      default_.equals(other.default_) &&
      annotations.equals(other.annotations);
  }

  @Override
  public int hashCode() {
    int result = 1;
    result = result * 37 + name.hashCode();
    result = result * 37 + typeExpr.hashCode();
    result = result * 37 + default_.hashCode();
    result = result * 37 + annotations.hashCode();
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
    final Lazy<JsonBinding<Optional<Literal>>> default_ = new Lazy<>(() -> MaybeHelpers.jsonBinding(Literal.jsonBinding()));
    final Lazy<JsonBinding<HashMap<ScopedName, Literal>>> annotations = new Lazy<>(() -> HashMapHelpers.jsonBinding(ScopedName.jsonBinding(), Literal.jsonBinding()));
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
        _result.add("annotations", annotations.get().toJson(_value.annotations));
        return _result;
      }

      public Field fromJson(JsonElement _json) {
        JsonObject _obj = _json.getAsJsonObject();
        return new Field(
          _obj.has("name") ? name.get().fromJson(_obj.get("name")) : "",
          _obj.has("typeExpr") ? typeExpr.get().fromJson(_obj.get("typeExpr")) : new TypeExpr(),
          _obj.has("default") ? default_.get().fromJson(_obj.get("default")) : MaybeHelpers.factory(Literal.FACTORY).create(),
          _obj.has("annotations") ? annotations.get().fromJson(_obj.get("annotations")) : HashMapHelpers.factory(ScopedName.FACTORY, Literal.FACTORY).create()
        );
      }
    };
  }
}
