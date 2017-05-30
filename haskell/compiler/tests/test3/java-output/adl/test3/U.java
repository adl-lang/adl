/* Code generated from adl module test3 */

package adl.test3;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonPrimitive;
import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
import org.adl.runtime.JsonParseException;
import org.adl.runtime.Lazy;
import java.util.Map;
import java.util.Objects;

public class U {

  /* Members */

  private Disc disc;
  private Object value;

  /**
   * The U discriminator type.
   */
  public enum Disc {
    F_INT,
    F_STRING,
    F_VOID
  }

  /* Constructors */

  public static U f_int(short v) {
    return new U(Disc.F_INT, v);
  }

  public static U f_string(String v) {
    return new U(Disc.F_STRING, Objects.requireNonNull(v));
  }

  public static U f_void() {
    return new U(Disc.F_VOID, null);
  }

  public U() {
    this.disc = Disc.F_INT;
    this.value = (short)0;
  }

  public U(U other) {
    this.disc = other.disc;
    switch (other.disc) {
      case F_INT:
        this.value = (Short) other.value;
        break;
      case F_STRING:
        this.value = (String) other.value;
        break;
      case F_VOID:
        this.value = (Void) other.value;
        break;
    }
  }

  private U(Disc disc, Object value) {
    this.disc = disc;
    this.value = value;
  }

  /* Accessors */

  public Disc getDisc() {
    return disc;
  }

  public short getF_int() {
    if (disc == Disc.F_INT) {
      return (Short) value;
    }
    throw new IllegalStateException();
  }

  public String getF_string() {
    if (disc == Disc.F_STRING) {
      return (String) value;
    }
    throw new IllegalStateException();
  }

  /* Mutators */

  public void setF_int(short v) {
    this.value = v;
    this.disc = Disc.F_INT;
  }

  public void setF_string(String v) {
    this.value = Objects.requireNonNull(v);
    this.disc = Disc.F_STRING;
  }

  public void setF_void() {
    this.value = null;
    this.disc = Disc.F_VOID;
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof U)) {
      return false;
    }
    U other = (U) other0;
    switch (disc) {
      case F_INT:
        return disc == other.disc && value.equals(other.value);
      case F_STRING:
        return disc == other.disc && value.equals(other.value);
      case F_VOID:
        return disc == other.disc;
    }
    throw new IllegalStateException();
  }

  @Override
  public int hashCode() {
    switch (disc) {
      case F_INT:
        return disc.hashCode() * 37 + value.hashCode();
      case F_STRING:
        return disc.hashCode() * 37 + value.hashCode();
      case F_VOID:
        return disc.hashCode();
    }
    throw new IllegalStateException();
  }

  /* Factory for construction of generic values */

  public static final Factory<U> FACTORY = new Factory<U>() {
    public U create() {
      return new U();
    }
    public U create(U other) {
      return new U(other);
    }
  };

  /* Json serialization */

  public static JsonBinding<U> jsonBinding() {
    final Lazy<JsonBinding<Short>> f_int = new Lazy<>(() -> JsonBindings.SHORT);
    final Lazy<JsonBinding<String>> f_string = new Lazy<>(() -> JsonBindings.STRING);
    final Lazy<JsonBinding<Void>> f_void = new Lazy<>(() -> JsonBindings.VOID);
    final Factory<U> _factory = FACTORY;

    return new JsonBinding<U>() {
      public Factory<U> factory() {
        return _factory;
      }

      public JsonElement toJson(U _value) {
        switch (_value.getDisc()) {
          case F_INT:
            return JsonBindings.unionToJson("f_int", _value.getF_int(), f_int.get());
          case F_STRING:
            return JsonBindings.unionToJson("f_string", _value.getF_string(), f_string.get());
          case F_VOID:
            return JsonBindings.unionToJson("f_void", null, null);
        }
        return null;
      }

      public U fromJson(JsonElement _json) {
        String _key = JsonBindings.unionNameFromJson(_json);
        if (_key.equals("f_int")) {
          return U.f_int(JsonBindings.unionValueFromJson(_json, f_int.get()));
        }
        else if (_key.equals("f_string")) {
          return U.f_string(JsonBindings.unionValueFromJson(_json, f_string.get()));
        }
        else if (_key.equals("f_void")) {
          return U.f_void();
        }
        throw new JsonParseException("Invalid discriminator " + _key + " for union U");
      }
    };
  }
}
