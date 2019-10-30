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

public class ScopedName {

  /* Members */

  private String moduleName;
  private String name;

  /* Constructors */

  public ScopedName(String moduleName, String name) {
    this.moduleName = Objects.requireNonNull(moduleName);
    this.name = Objects.requireNonNull(name);
  }

  public ScopedName() {
    this.moduleName = "";
    this.name = "";
  }

  public ScopedName(ScopedName other) {
    this.moduleName = other.moduleName;
    this.name = other.name;
  }

  /* Accessors and mutators */

  public String getModuleName() {
    return moduleName;
  }

  public void setModuleName(String moduleName) {
    this.moduleName = Objects.requireNonNull(moduleName);
  }

  public String getName() {
    return name;
  }

  public void setName(String name) {
    this.name = Objects.requireNonNull(name);
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof ScopedName)) {
      return false;
    }
    ScopedName other = (ScopedName) other0;
    return
      moduleName.equals(other.moduleName) &&
      name.equals(other.name);
  }

  @Override
  public int hashCode() {
    int _result = 1;
    _result = _result * 37 + moduleName.hashCode();
    _result = _result * 37 + name.hashCode();
    return _result;
  }

  /* Builder */

  public static class Builder {
    private String moduleName;
    private String name;

    public Builder() {
      this.moduleName = null;
      this.name = null;
    }

    public Builder setModuleName(String moduleName) {
      this.moduleName = Objects.requireNonNull(moduleName);
      return this;
    }

    public Builder setName(String name) {
      this.name = Objects.requireNonNull(name);
      return this;
    }

    public ScopedName create() {
      Builders.checkFieldInitialized("ScopedName", "moduleName", moduleName);
      Builders.checkFieldInitialized("ScopedName", "name", name);
      return new ScopedName(moduleName, name);
    }
  }

  /* Factory for construction of generic values */

  public static final Factory<ScopedName> FACTORY = new Factory<ScopedName>() {
    @Override
    public ScopedName create() {
      return new ScopedName();
    }

    @Override
    public ScopedName create(ScopedName other) {
      return new ScopedName(other);
    }

    @Override
    public TypeExpr typeExpr() {
      org.adl.runtime.sys.adlast.ScopedName scopedName = new org.adl.runtime.sys.adlast.ScopedName("sys.adlast", "ScopedName");
      ArrayList<TypeExpr> params = new ArrayList<>();
      return new TypeExpr(TypeRef.reference(scopedName), params);
    }
    @Override
    public JsonBinding<ScopedName> jsonBinding() {
      return ScopedName.jsonBinding();
    }
  };

  /* Json serialization */

  public static JsonBinding<ScopedName> jsonBinding() {
    final Lazy<JsonBinding<String>> moduleName = new Lazy<>(() -> JsonBindings.STRING);
    final Lazy<JsonBinding<String>> name = new Lazy<>(() -> JsonBindings.STRING);
    final Factory<ScopedName> _factory = FACTORY;

    return new JsonBinding<ScopedName>() {
      @Override
      public Factory<ScopedName> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(ScopedName _value) {
        JsonObject _result = new JsonObject();
        _result.add("moduleName", moduleName.get().toJson(_value.moduleName));
        _result.add("name", name.get().toJson(_value.name));
        return _result;
      }

      @Override
      public ScopedName fromJson(JsonElement _json) {
        JsonObject _obj = JsonBindings.objectFromJson(_json);
        return new ScopedName(
          JsonBindings.fieldFromJson(_obj, "moduleName", moduleName.get()),
          JsonBindings.fieldFromJson(_obj, "name", name.get())
        );
      }
    };
  }
}
