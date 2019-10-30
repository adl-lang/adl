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

public class Decl {

  /* Members */

  private String name;
  private Optional<Integer> version;
  private DeclType type_;
  private HashMap<ScopedName, JsonElement> annotations;

  /* Constructors */

  public Decl(String name, Optional<Integer> version, DeclType type_, HashMap<ScopedName, JsonElement> annotations) {
    this.name = Objects.requireNonNull(name);
    this.version = Objects.requireNonNull(version);
    this.type_ = Objects.requireNonNull(type_);
    this.annotations = Objects.requireNonNull(annotations);
  }

  public Decl() {
    this.name = "";
    this.version = MaybeHelpers.factory(Factories.WORD32).create();
    this.type_ = new DeclType();
    this.annotations = HashMapHelpers.factory(ScopedName.FACTORY, JsonBindings.JSON_FACTORY).create();
  }

  public Decl(Decl other) {
    this.name = other.name;
    this.version = MaybeHelpers.factory(Factories.WORD32).create(other.version);
    this.type_ = DeclType.FACTORY.create(other.type_);
    this.annotations = HashMapHelpers.factory(ScopedName.FACTORY, JsonBindings.JSON_FACTORY).create(other.annotations);
  }

  /* Accessors and mutators */

  public String getName() {
    return name;
  }

  public void setName(String name) {
    this.name = Objects.requireNonNull(name);
  }

  public Optional<Integer> getVersion() {
    return version;
  }

  public void setVersion(Optional<Integer> version) {
    this.version = Objects.requireNonNull(version);
  }

  public DeclType getType_() {
    return type_;
  }

  public void setType_(DeclType type_) {
    this.type_ = Objects.requireNonNull(type_);
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
    if (!(other0 instanceof Decl)) {
      return false;
    }
    Decl other = (Decl) other0;
    return
      name.equals(other.name) &&
      version.equals(other.version) &&
      type_.equals(other.type_) &&
      annotations.equals(other.annotations);
  }

  @Override
  public int hashCode() {
    int _result = 1;
    _result = _result * 37 + name.hashCode();
    _result = _result * 37 + version.hashCode();
    _result = _result * 37 + type_.hashCode();
    _result = _result * 37 + annotations.hashCode();
    return _result;
  }

  /* Builder */

  public static class Builder {
    private String name;
    private Optional<Integer> version;
    private DeclType type_;
    private HashMap<ScopedName, JsonElement> annotations;

    public Builder() {
      this.name = null;
      this.version = null;
      this.type_ = null;
      this.annotations = null;
    }

    public Builder setName(String name) {
      this.name = Objects.requireNonNull(name);
      return this;
    }

    public Builder setVersion(Optional<Integer> version) {
      this.version = Objects.requireNonNull(version);
      return this;
    }

    public Builder setType_(DeclType type_) {
      this.type_ = Objects.requireNonNull(type_);
      return this;
    }

    public Builder setAnnotations(HashMap<ScopedName, JsonElement> annotations) {
      this.annotations = Objects.requireNonNull(annotations);
      return this;
    }

    public Decl create() {
      Builders.checkFieldInitialized("Decl", "name", name);
      Builders.checkFieldInitialized("Decl", "version", version);
      Builders.checkFieldInitialized("Decl", "type_", type_);
      Builders.checkFieldInitialized("Decl", "annotations", annotations);
      return new Decl(name, version, type_, annotations);
    }
  }

  /* Factory for construction of generic values */

  public static final Factory<Decl> FACTORY = new Factory<Decl>() {
    @Override
    public Decl create() {
      return new Decl();
    }

    @Override
    public Decl create(Decl other) {
      return new Decl(other);
    }

    @Override
    public TypeExpr typeExpr() {
      ScopedName scopedName = new ScopedName("sys.adlast", "Decl");
      ArrayList<TypeExpr> params = new ArrayList<>();
      return new TypeExpr(TypeRef.reference(scopedName), params);
    }
    @Override
    public JsonBinding<Decl> jsonBinding() {
      return Decl.jsonBinding();
    }
  };

  /* Json serialization */

  public static JsonBinding<Decl> jsonBinding() {
    final Lazy<JsonBinding<String>> name = new Lazy<>(() -> JsonBindings.STRING);
    final Lazy<JsonBinding<Optional<Integer>>> version = new Lazy<>(() -> MaybeHelpers.jsonBinding(JsonBindings.WORD32));
    final Lazy<JsonBinding<DeclType>> type_ = new Lazy<>(() -> DeclType.jsonBinding());
    final Lazy<JsonBinding<HashMap<ScopedName, JsonElement>>> annotations = new Lazy<>(() -> HashMapHelpers.jsonBinding(ScopedName.jsonBinding(), JsonBindings.JSON));
    final Factory<Decl> _factory = FACTORY;

    return new JsonBinding<Decl>() {
      @Override
      public Factory<Decl> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(Decl _value) {
        JsonObject _result = new JsonObject();
        _result.add("name", name.get().toJson(_value.name));
        _result.add("version", version.get().toJson(_value.version));
        _result.add("type_", type_.get().toJson(_value.type_));
        _result.add("annotations", annotations.get().toJson(_value.annotations));
        return _result;
      }

      @Override
      public Decl fromJson(JsonElement _json) {
        JsonObject _obj = JsonBindings.objectFromJson(_json);
        return new Decl(
          JsonBindings.fieldFromJson(_obj, "name", name.get()),
          JsonBindings.fieldFromJson(_obj, "version", version.get()),
          JsonBindings.fieldFromJson(_obj, "type_", type_.get()),
          JsonBindings.fieldFromJson(_obj, "annotations", annotations.get())
        );
      }
    };
  }
}
