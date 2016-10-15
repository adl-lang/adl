package adl.test3;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
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
    F_STRING
  }

  /* Constructors */

  public static U f_int(short v) {
    return new U(Disc.F_INT, v);
  }

  public static U f_string(String v) {
    return new U(Disc.F_STRING, Objects.requireNonNull(v));
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

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof U)) {
      return false;
    }
    U other = (U) other0;
    return disc == other.disc && value.equals(other.value);
  }

  @Override
  public int hashCode() {
    return disc.hashCode() * 37 + value.hashCode();
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
    final JsonBinding<Short> f_int = JsonBindings.SHORT;
    final JsonBinding<String> f_string = JsonBindings.STRING;
    final Factory<U> _factory = FACTORY;

    return new JsonBinding<U>() {
      public Factory<U> factory() {
        return _factory;
      }

      public JsonElement toJson(U _value) {
        JsonObject _result = new JsonObject();
        switch (_value.getDisc()) {
          case F_INT:
            _result.add("f_int", f_int.toJson(_value.getF_int()));
            break;
          case F_STRING:
            _result.add("f_string", f_string.toJson(_value.getF_string()));
            break;
        }
        return _result;
      }

      public U fromJson(JsonElement _json) {
        JsonObject _obj = _json.getAsJsonObject();
        for (Map.Entry<String,JsonElement> _v : _obj.entrySet()) {
          if (_v.getKey().equals("f_int")) {
            return U.f_int(f_int.fromJson(_v.getValue()));
          }
          else if (_v.getKey().equals("f_string")) {
            return U.f_string(f_string.fromJson(_v.getValue()));
          }
        }
        throw new IllegalStateException();
      }
    };
  }
}
