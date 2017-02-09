/* Code generated from adl module sys.adlast */

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

public class Decl {

  /* Members */

  private String name;
  private Optional<Integer> version;
  private DeclType type_;
  private HashMap<ScopedName, Literal> annotations;

  /* Constructors */

  public Decl(String name, Optional<Integer> version, DeclType type_, HashMap<ScopedName, Literal> annotations) {
    this.name = Objects.requireNonNull(name);
    this.version = Objects.requireNonNull(version);
    this.type_ = Objects.requireNonNull(type_);
    this.annotations = Objects.requireNonNull(annotations);
  }

  public Decl() {
    this.name = "";
    this.version = MaybeHelpers.factory(Factories.INTEGER).create();
    this.type_ = new DeclType();
    this.annotations = HashMapHelpers.factory(ScopedName.FACTORY, Literal.FACTORY).create();
  }

  public Decl(Decl other) {
    this.name = other.name;
    this.version = MaybeHelpers.factory(Factories.INTEGER).create(other.version);
    this.type_ = DeclType.FACTORY.create(other.type_);
    this.annotations = HashMapHelpers.factory(ScopedName.FACTORY, Literal.FACTORY).create(other.annotations);
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

  public HashMap<ScopedName, Literal> getAnnotations() {
    return annotations;
  }

  public void setAnnotations(HashMap<ScopedName, Literal> annotations) {
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
    int result = 1;
    result = result * 37 + name.hashCode();
    result = result * 37 + version.hashCode();
    result = result * 37 + type_.hashCode();
    result = result * 37 + annotations.hashCode();
    return result;
  }

  /* Factory for construction of generic values */

  public static final Factory<Decl> FACTORY = new Factory<Decl>() {
    public Decl create() {
      return new Decl();
    }
    public Decl create(Decl other) {
      return new Decl(other);
    }
  };

  /* Json serialization */

  public static JsonBinding<Decl> jsonBinding() {
    final Lazy<JsonBinding<String>> name = new Lazy<>(() -> JsonBindings.STRING);
    final Lazy<JsonBinding<Optional<Integer>>> version = new Lazy<>(() -> MaybeHelpers.jsonBinding(JsonBindings.INTEGER));
    final Lazy<JsonBinding<DeclType>> type_ = new Lazy<>(() -> DeclType.jsonBinding());
    final Lazy<JsonBinding<HashMap<ScopedName, Literal>>> annotations = new Lazy<>(() -> HashMapHelpers.jsonBinding(ScopedName.jsonBinding(), Literal.jsonBinding()));
    final Factory<Decl> _factory = FACTORY;

    return new JsonBinding<Decl>() {
      public Factory<Decl> factory() {
        return _factory;
      }

      public JsonElement toJson(Decl _value) {
        JsonObject _result = new JsonObject();
        _result.add("name", name.get().toJson(_value.name));
        _result.add("version", version.get().toJson(_value.version));
        _result.add("type_", type_.get().toJson(_value.type_));
        _result.add("annotations", annotations.get().toJson(_value.annotations));
        return _result;
      }

      public Decl fromJson(JsonElement _json) {
        JsonObject _obj = _json.getAsJsonObject();
        return new Decl(
          _obj.has("name") ? name.get().fromJson(_obj.get("name")) : "",
          _obj.has("version") ? version.get().fromJson(_obj.get("version")) : MaybeHelpers.factory(Factories.INTEGER).create(),
          _obj.has("type_") ? type_.get().fromJson(_obj.get("type_")) : new DeclType(),
          _obj.has("annotations") ? annotations.get().fromJson(_obj.get("annotations")) : HashMapHelpers.factory(ScopedName.FACTORY, Literal.FACTORY).create()
        );
      }
    };
  }
}
