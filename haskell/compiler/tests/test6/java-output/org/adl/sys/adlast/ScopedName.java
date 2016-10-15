package org.adl.sys.adlast;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
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
    int result = 1;
    result = result * 37 + moduleName.hashCode();
    result = result * 37 + name.hashCode();
    return result;
  }

  /* Factory for construction of generic values */

  public static final Factory<ScopedName> FACTORY = new Factory<ScopedName>() {
    public ScopedName create() {
      return new ScopedName();
    }
    public ScopedName create(ScopedName other) {
      return new ScopedName(other);
    }
  };

  /* Json serialization */

  public static JsonBinding<ScopedName> jsonBinding() {
    final JsonBinding<String> moduleName = JsonBindings.STRING;
    final JsonBinding<String> name = JsonBindings.STRING;
    final Factory<ScopedName> _factory = FACTORY;

    return new JsonBinding<ScopedName>() {
      public Factory<ScopedName> factory() {
        return _factory;
      }

      public JsonElement toJson(ScopedName _value) {
        JsonObject _result = new JsonObject();
        _result.add("moduleName", moduleName.toJson(_value.moduleName));
        _result.add("name", name.toJson(_value.name));
        return _result;
      }

      public ScopedName fromJson(JsonElement _json) {
        JsonObject _obj = _json.getAsJsonObject();
        return new ScopedName(
          _obj.has("moduleName") ? moduleName.fromJson(_obj.get("moduleName")) : "",
          _obj.has("name") ? name.fromJson(_obj.get("name")) : ""
        );
      }
    };
  }
}
