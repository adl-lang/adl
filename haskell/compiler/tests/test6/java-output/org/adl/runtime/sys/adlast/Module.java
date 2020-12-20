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
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

public class Module {

  /* Members */

  private String name;
  private List<Import> imports;
  private Map<String, Decl> decls;
  private Map<ScopedName, JsonElement> annotations;

  /* Constructors */

  public Module(String name, List<Import> imports, Map<String, Decl> decls, Map<ScopedName, JsonElement> annotations) {
    this.name = Objects.requireNonNull(name);
    this.imports = Objects.requireNonNull(imports);
    this.decls = Objects.requireNonNull(decls);
    this.annotations = Objects.requireNonNull(annotations);
  }

  public Module(Module other) {
    this.name = other.name;
    this.imports = Factories.list(Import.FACTORY).create(other.imports);
    this.decls = Factories.stringMap(Decl.FACTORY).create(other.decls);
    this.annotations = HashMapHelpers.factory(ScopedName.FACTORY, JsonBindings.JSON_FACTORY).create(other.annotations);
  }

  /* Accessors and mutators */

  public String getName() {
    return name;
  }

  public void setName(String name) {
    this.name = Objects.requireNonNull(name);
  }

  public List<Import> getImports() {
    return imports;
  }

  public void setImports(List<Import> imports) {
    this.imports = Objects.requireNonNull(imports);
  }

  public Map<String, Decl> getDecls() {
    return decls;
  }

  public void setDecls(Map<String, Decl> decls) {
    this.decls = Objects.requireNonNull(decls);
  }

  public Map<ScopedName, JsonElement> getAnnotations() {
    return annotations;
  }

  public void setAnnotations(Map<ScopedName, JsonElement> annotations) {
    this.annotations = Objects.requireNonNull(annotations);
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof Module)) {
      return false;
    }
    Module other = (Module) other0;
    return
      name.equals(other.name) &&
      imports.equals(other.imports) &&
      decls.equals(other.decls) &&
      annotations.equals(other.annotations);
  }

  @Override
  public int hashCode() {
    int _result = 1;
    _result = _result * 37 + name.hashCode();
    _result = _result * 37 + imports.hashCode();
    _result = _result * 37 + decls.hashCode();
    _result = _result * 37 + annotations.hashCode();
    return _result;
  }

  /* Builder */

  public static class Builder {
    private String name;
    private List<Import> imports;
    private Map<String, Decl> decls;
    private Map<ScopedName, JsonElement> annotations;

    public Builder() {
      this.name = null;
      this.imports = null;
      this.decls = null;
      this.annotations = null;
    }

    public Builder setName(String name) {
      this.name = Objects.requireNonNull(name);
      return this;
    }

    public Builder setImports(List<Import> imports) {
      this.imports = Objects.requireNonNull(imports);
      return this;
    }

    public Builder setDecls(Map<String, Decl> decls) {
      this.decls = Objects.requireNonNull(decls);
      return this;
    }

    public Builder setAnnotations(Map<ScopedName, JsonElement> annotations) {
      this.annotations = Objects.requireNonNull(annotations);
      return this;
    }

    public Module create() {
      Builders.checkFieldInitialized("Module", "name", name);
      Builders.checkFieldInitialized("Module", "imports", imports);
      Builders.checkFieldInitialized("Module", "decls", decls);
      Builders.checkFieldInitialized("Module", "annotations", annotations);
      return new Module(name, imports, decls, annotations);
    }
  }

  /* Factory for construction of generic values */

  public static final Factory<Module> FACTORY = new Factory<Module>() {
    @Override
    public Module create(Module other) {
      return new Module(other);
    }

    @Override
    public TypeExpr typeExpr() {
      ScopedName scopedName = new ScopedName("sys.adlast", "Module");
      ArrayList<TypeExpr> params = new ArrayList<>();
      return new TypeExpr(TypeRef.reference(scopedName), params);
    }
    @Override
    public JsonBinding<Module> jsonBinding() {
      return Module.jsonBinding();
    }
  };

  /* Json serialization */

  public static JsonBinding<Module> jsonBinding() {
    final Lazy<JsonBinding<String>> name = new Lazy<>(() -> JsonBindings.STRING);
    final Lazy<JsonBinding<List<Import>>> imports = new Lazy<>(() -> JsonBindings.list(Import.jsonBinding()));
    final Lazy<JsonBinding<Map<String, Decl>>> decls = new Lazy<>(() -> JsonBindings.stringMap(Decl.jsonBinding()));
    final Lazy<JsonBinding<Map<ScopedName, JsonElement>>> annotations = new Lazy<>(() -> HashMapHelpers.jsonBinding(ScopedName.jsonBinding(), JsonBindings.JSON));
    final Factory<Module> _factory = FACTORY;

    return new JsonBinding<Module>() {
      @Override
      public Factory<Module> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(Module _value) {
        JsonObject _result = new JsonObject();
        _result.add("name", name.get().toJson(_value.name));
        _result.add("imports", imports.get().toJson(_value.imports));
        _result.add("decls", decls.get().toJson(_value.decls));
        _result.add("annotations", annotations.get().toJson(_value.annotations));
        return _result;
      }

      @Override
      public Module fromJson(JsonElement _json) {
        JsonObject _obj = JsonBindings.objectFromJson(_json);
        return new Module(
          JsonBindings.fieldFromJson(_obj, "name", name.get()),
          JsonBindings.fieldFromJson(_obj, "imports", imports.get()),
          JsonBindings.fieldFromJson(_obj, "decls", decls.get()),
          JsonBindings.fieldFromJson(_obj, "annotations", annotations.get())
        );
      }
    };
  }
}
