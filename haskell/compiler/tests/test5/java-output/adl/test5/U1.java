package adl.test5;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
import java.util.Map;
import java.util.Objects;

public class U1 {

  /* Members */

  private Disc disc;
  private Object value;

  /**
   * The U1 discriminator type.
   */
  public enum Disc {
    V
  }

  /* Constructors */

  public static U1 v() {
    return new U1(Disc.V, null);
  }

  public U1() {
    this.disc = Disc.V;
    this.value = null;
  }

  public U1(U1 other) {
    this.disc = other.disc;
    switch (other.disc) {
      case V:
        this.value = (Void) other.value;
        break;
    }
  }

  private U1(Disc disc, Object value) {
    this.disc = disc;
    this.value = value;
  }

  /* Accessors */

  public Disc getDisc() {
    return disc;
  }

  /* Mutators */

  public void setV() {
    this.value = null;
    this.disc = Disc.V;
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof U1)) {
      return false;
    }
    U1 other = (U1) other0;
    return disc == other.disc;
  }

  @Override
  public int hashCode() {
    return disc.hashCode();
  }

  /* Factory for construction of generic values */

  public static final Factory<U1> FACTORY = new Factory<U1>() {
    public U1 create() {
      return new U1();
    }
    public U1 create(U1 other) {
      return new U1(other);
    }
  };

  /* Json serialization */

  public static JsonBinding<U1> jsonBinding() {
    final JsonBinding<Void> v = JsonBindings.VOID;
    final Factory<U1> _factory = FACTORY;

    return new JsonBinding<U1>() {
      public Factory<U1> factory() {
        return _factory;
      }

      public JsonElement toJson(U1 _value) {
        JsonObject _result = new JsonObject();
        switch (_value.getDisc()) {
          case V:
            _result.add("v", null);
        }
        return _result;
      }

      public U1 fromJson(JsonElement _json) {
        JsonObject _obj = _json.getAsJsonObject();
        for (Map.Entry<String,JsonElement> _v : _obj.entrySet()) {
          if (_v.getKey().equals("v")) {
            return U1.v();
          }
        }
        throw new IllegalStateException();
      }
    };
  }
}
