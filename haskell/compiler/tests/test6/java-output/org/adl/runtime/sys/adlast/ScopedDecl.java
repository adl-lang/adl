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

public class ScopedDecl {

  /* Members */

  private String moduleName;
  private Decl decl;

  /* Constructors */

  public ScopedDecl(String moduleName, Decl decl) {
    this.moduleName = Objects.requireNonNull(moduleName);
    this.decl = Objects.requireNonNull(decl);
  }

  public ScopedDecl() {
    this.moduleName = "";
    this.decl = new Decl();
  }

  public ScopedDecl(ScopedDecl other) {
    this.moduleName = other.moduleName;
    this.decl = Decl.FACTORY.create(other.decl);
  }

  /* Accessors and mutators */

  public String getModuleName() {
    return moduleName;
  }

  public void setModuleName(String moduleName) {
    this.moduleName = Objects.requireNonNull(moduleName);
  }

  public Decl getDecl() {
    return decl;
  }

  public void setDecl(Decl decl) {
    this.decl = Objects.requireNonNull(decl);
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof ScopedDecl)) {
      return false;
    }
    ScopedDecl other = (ScopedDecl) other0;
    return
      moduleName.equals(other.moduleName) &&
      decl.equals(other.decl);
  }

  @Override
  public int hashCode() {
    int _result = 1;
    _result = _result * 37 + moduleName.hashCode();
    _result = _result * 37 + decl.hashCode();
    return _result;
  }

  /* Builder */

  public static class Builder {
    private String moduleName;
    private Decl decl;

    public Builder() {
      this.moduleName = null;
      this.decl = null;
    }

    public Builder setModuleName(String moduleName) {
      this.moduleName = Objects.requireNonNull(moduleName);
      return this;
    }

    public Builder setDecl(Decl decl) {
      this.decl = Objects.requireNonNull(decl);
      return this;
    }

    public ScopedDecl create() {
      Builders.checkFieldInitialized("ScopedDecl", "moduleName", moduleName);
      Builders.checkFieldInitialized("ScopedDecl", "decl", decl);
      return new ScopedDecl(moduleName, decl);
    }
  }

  /* Factory for construction of generic values */

  public static final Factory<ScopedDecl> FACTORY = new Factory<ScopedDecl>() {
    @Override
    public ScopedDecl create() {
      return new ScopedDecl();
    }

    @Override
    public ScopedDecl create(ScopedDecl other) {
      return new ScopedDecl(other);
    }

    @Override
    public TypeExpr typeExpr() {
      ScopedName scopedName = new ScopedName("sys.adlast", "ScopedDecl");
      ArrayList<TypeExpr> params = new ArrayList<>();
      return new TypeExpr(TypeRef.reference(scopedName), params);
    }
    @Override
    public JsonBinding<ScopedDecl> jsonBinding() {
      return ScopedDecl.jsonBinding();
    }
  };

  /* Json serialization */

  public static JsonBinding<ScopedDecl> jsonBinding() {
    final Lazy<JsonBinding<String>> moduleName = new Lazy<>(() -> JsonBindings.STRING);
    final Lazy<JsonBinding<Decl>> decl = new Lazy<>(() -> Decl.jsonBinding());
    final Factory<ScopedDecl> _factory = FACTORY;

    return new JsonBinding<ScopedDecl>() {
      @Override
      public Factory<ScopedDecl> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(ScopedDecl _value) {
        JsonObject _result = new JsonObject();
        _result.add("moduleName", moduleName.get().toJson(_value.moduleName));
        _result.add("decl", decl.get().toJson(_value.decl));
        return _result;
      }

      @Override
      public ScopedDecl fromJson(JsonElement _json) {
        JsonObject _obj = JsonBindings.objectFromJson(_json);
        return new ScopedDecl(
          JsonBindings.fieldFromJson(_obj, "moduleName", moduleName.get()),
          JsonBindings.fieldFromJson(_obj, "decl", decl.get())
        );
      }
    };
  }
}
