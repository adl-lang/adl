/* @generated from adl module sys.adlast */

package org.adl.runtime.sys.adlast;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.adl.runtime.Builders;
import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import org.adl.runtime.HashMapHelpers;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
import org.adl.runtime.Lazy;
import org.adl.runtime.MaybeHelpers;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Objects;
import java.util.Optional;

public class Field {

  /* Members */

  private String name;
  private String serializedName;
  private TypeExpr typeExpr;
  private Optional<JsonElement> default_;
  private HashMap<ScopedName, JsonElement> annotations;

  /* Constructors */

  public Field(String name, String serializedName, TypeExpr typeExpr, Optional<JsonElement> default_, HashMap<ScopedName, JsonElement> annotations) {
    this.name = Objects.requireNonNull(name);
    this.serializedName = Objects.requireNonNull(serializedName);
    this.typeExpr = Objects.requireNonNull(typeExpr);
    this.default_ = Objects.requireNonNull(default_);
    this.annotations = Objects.requireNonNull(annotations);
  }

  public Field() {
    this.name = "";
    this.serializedName = "";
    this.typeExpr = new TypeExpr();
    this.default_ = MaybeHelpers.factory(JsonBindings.JSON_FACTORY).create();
    this.annotations = HashMapHelpers.factory(ScopedName.FACTORY, JsonBindings.JSON_FACTORY).create();
  }

  public Field(Field other) {
    this.name = other.name;
    this.serializedName = other.serializedName;
    this.typeExpr = TypeExpr.FACTORY.create(other.typeExpr);
    this.default_ = MaybeHelpers.factory(JsonBindings.JSON_FACTORY).create(other.default_);
    this.annotations = HashMapHelpers.factory(ScopedName.FACTORY, JsonBindings.JSON_FACTORY).create(other.annotations);
  }

  /* Accessors and mutators */

  public String getName() {
    return name;
  }

  public void setName(String name) {
    this.name = Objects.requireNonNull(name);
  }

  public String getSerializedName() {
    return serializedName;
  }

  public void setSerializedName(String serializedName) {
    this.serializedName = Objects.requireNonNull(serializedName);
  }

  public TypeExpr getTypeExpr() {
    return typeExpr;
  }

  public void setTypeExpr(TypeExpr typeExpr) {
    this.typeExpr = Objects.requireNonNull(typeExpr);
  }

  public Optional<JsonElement> getDefault() {
    return default_;
  }

  public void setDefault(Optional<JsonElement> default_) {
    this.default_ = Objects.requireNonNull(default_);
  }

  public HashMap<ScopedName, JsonElement> getAnnotations() {
    return annotations;
  }

  public void setAnnotations(HashMap<ScopedName, JsonElement> annotations) {
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
      serializedName.equals(other.serializedName) &&
      typeExpr.equals(other.typeExpr) &&
      default_.equals(other.default_) &&
      annotations.equals(other.annotations);
  }

  @Override
  public int hashCode() {
    int _result = 1;
    _result = _result * 37 + name.hashCode();
    _result = _result * 37 + serializedName.hashCode();
    _result = _result * 37 + typeExpr.hashCode();
    _result = _result * 37 + default_.hashCode();
    _result = _result * 37 + annotations.hashCode();
    return _result;
  }

  /* Builder */

  public static class Builder {
    private String name;
    private String serializedName;
    private TypeExpr typeExpr;
    private Optional<JsonElement> default_;
    private HashMap<ScopedName, JsonElement> annotations;

    public Builder() {
      this.name = null;
      this.serializedName = null;
      this.typeExpr = null;
      this.default_ = null;
      this.annotations = null;
    }

    public Builder setName(String name) {
      this.name = Objects.requireNonNull(name);
      return this;
    }

    public Builder setSerializedName(String serializedName) {
      this.serializedName = Objects.requireNonNull(serializedName);
      return this;
    }

    public Builder setTypeExpr(TypeExpr typeExpr) {
      this.typeExpr = Objects.requireNonNull(typeExpr);
      return this;
    }

    public Builder setDefault(Optional<JsonElement> default_) {
      this.default_ = Objects.requireNonNull(default_);
      return this;
    }

    public Builder setAnnotations(HashMap<ScopedName, JsonElement> annotations) {
      this.annotations = Objects.requireNonNull(annotations);
      return this;
    }

    public Field create() {
      Builders.checkFieldInitialized("Field", "name", name);
      Builders.checkFieldInitialized("Field", "serializedName", serializedName);
      Builders.checkFieldInitialized("Field", "typeExpr", typeExpr);
      Builders.checkFieldInitialized("Field", "default_", default_);
      Builders.checkFieldInitialized("Field", "annotations", annotations);
      return new Field(name, serializedName, typeExpr, default_, annotations);
    }
  }

  /* Factory for construction of generic values */

  public static final Factory<Field> FACTORY = new Factory<Field>() {
    @Override
    public Field create() {
      return new Field();
    }

    @Override
    public Field create(Field other) {
      return new Field(other);
    }

    @Override
    public TypeExpr typeExpr() {
      ScopedName scopedName = new ScopedName("sys.adlast", "Field");
      ArrayList<TypeExpr> params = new ArrayList<>();
      return new TypeExpr(TypeRef.reference(scopedName), params);
    }
    @Override
    public JsonBinding<Field> jsonBinding() {
      return Field.jsonBinding();
    }
  };

  /* Json serialization */

  public static JsonBinding<Field> jsonBinding() {
    final Lazy<JsonBinding<String>> name = new Lazy<>(() -> JsonBindings.STRING);
    final Lazy<JsonBinding<String>> serializedName = new Lazy<>(() -> JsonBindings.STRING);
    final Lazy<JsonBinding<TypeExpr>> typeExpr = new Lazy<>(() -> TypeExpr.jsonBinding());
    final Lazy<JsonBinding<Optional<JsonElement>>> default_ = new Lazy<>(() -> MaybeHelpers.jsonBinding(JsonBindings.JSON));
    final Lazy<JsonBinding<HashMap<ScopedName, JsonElement>>> annotations = new Lazy<>(() -> HashMapHelpers.jsonBinding(ScopedName.jsonBinding(), JsonBindings.JSON));
    final Factory<Field> _factory = FACTORY;

    return new JsonBinding<Field>() {
      @Override
      public Factory<Field> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(Field _value) {
        JsonObject _result = new JsonObject();
        _result.add("name", name.get().toJson(_value.name));
        _result.add("serializedName", serializedName.get().toJson(_value.serializedName));
        _result.add("typeExpr", typeExpr.get().toJson(_value.typeExpr));
        _result.add("default", default_.get().toJson(_value.default_));
        _result.add("annotations", annotations.get().toJson(_value.annotations));
        return _result;
      }

      @Override
      public Field fromJson(JsonElement _json) {
        JsonObject _obj = JsonBindings.objectFromJson(_json);
        return new Field(
          JsonBindings.fieldFromJson(_obj, "name", name.get()),
          JsonBindings.fieldFromJson(_obj, "serializedName", serializedName.get()),
          JsonBindings.fieldFromJson(_obj, "typeExpr", typeExpr.get()),
          JsonBindings.fieldFromJson(_obj, "default", default_.get()),
          JsonBindings.fieldFromJson(_obj, "annotations", annotations.get())
        );
      }
    };
  }
}
