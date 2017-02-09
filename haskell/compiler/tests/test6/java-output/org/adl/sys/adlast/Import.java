/* Code generated from adl module sys.adlast */

package org.adl.sys.adlast;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonPrimitive;
import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
import org.adl.runtime.Lazy;
import java.util.Map;
import java.util.Objects;

public class Import {

  /* Members */

  private Disc disc;
  private Object value;

  /**
   * The Import discriminator type.
   */
  public enum Disc {
    MODULENAME,
    SCOPEDNAME
  }

  /* Constructors */

  public static Import moduleName(String v) {
    return new Import(Disc.MODULENAME, Objects.requireNonNull(v));
  }

  public static Import scopedName(ScopedName v) {
    return new Import(Disc.SCOPEDNAME, Objects.requireNonNull(v));
  }

  public Import() {
    this.disc = Disc.MODULENAME;
    this.value = "";
  }

  public Import(Import other) {
    this.disc = other.disc;
    switch (other.disc) {
      case MODULENAME:
        this.value = (String) other.value;
        break;
      case SCOPEDNAME:
        this.value = ScopedName.FACTORY.create((ScopedName) other.value);
        break;
    }
  }

  private Import(Disc disc, Object value) {
    this.disc = disc;
    this.value = value;
  }

  /* Accessors */

  public Disc getDisc() {
    return disc;
  }

  public String getModuleName() {
    if (disc == Disc.MODULENAME) {
      return (String) value;
    }
    throw new IllegalStateException();
  }

  public ScopedName getScopedName() {
    if (disc == Disc.SCOPEDNAME) {
      return (ScopedName) value;
    }
    throw new IllegalStateException();
  }

  /* Mutators */

  public void setModuleName(String v) {
    this.value = Objects.requireNonNull(v);
    this.disc = Disc.MODULENAME;
  }

  public void setScopedName(ScopedName v) {
    this.value = Objects.requireNonNull(v);
    this.disc = Disc.SCOPEDNAME;
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof Import)) {
      return false;
    }
    Import other = (Import) other0;
    return disc == other.disc && value.equals(other.value);
  }

  @Override
  public int hashCode() {
    return disc.hashCode() * 37 + value.hashCode();
  }

  /* Factory for construction of generic values */

  public static final Factory<Import> FACTORY = new Factory<Import>() {
    public Import create() {
      return new Import();
    }
    public Import create(Import other) {
      return new Import(other);
    }
  };

  /* Json serialization */

  public static JsonBinding<Import> jsonBinding() {
    final Lazy<JsonBinding<String>> moduleName = new Lazy<>(() -> JsonBindings.STRING);
    final Lazy<JsonBinding<ScopedName>> scopedName = new Lazy<>(() -> ScopedName.jsonBinding());
    final Factory<Import> _factory = FACTORY;

    return new JsonBinding<Import>() {
      public Factory<Import> factory() {
        return _factory;
      }

      public JsonElement toJson(Import _value) {
        switch (_value.getDisc()) {
          case MODULENAME:
            return JsonBindings.unionToJson("moduleName", _value.getModuleName(), moduleName.get());
          case SCOPEDNAME:
            return JsonBindings.unionToJson("scopedName", _value.getScopedName(), scopedName.get());
        }
        return null;
      }

      public Import fromJson(JsonElement _json) {
        String _key = JsonBindings.unionNameFromJson(_json);
        if (_key.equals("moduleName")) {
          return Import.moduleName(JsonBindings.unionValueFromJson(_json, moduleName.get()));
        }
        else if (_key.equals("scopedName")) {
          return Import.scopedName(JsonBindings.unionValueFromJson(_json, scopedName.get()));
        }
        throw new IllegalStateException();
      }
    };
  }
}
