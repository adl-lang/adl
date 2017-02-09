/* Code generated from adl module test5 */

package adl.test5;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonPrimitive;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
import org.adl.runtime.Lazy;
import java.util.Map;
import java.util.Objects;

public class U6 {

  /* Members */

  private Disc disc;
  private Object value;

  /**
   * The U6 discriminator type.
   */
  public enum Disc {
    V
  }

  /* Constructors */

  public static U6 v(U3 v) {
    return new U6(Disc.V, Objects.requireNonNull(v));
  }

  public U6() {
    this.disc = Disc.V;
    this.value = new U3();
  }

  public U6(U6 other) {
    this.disc = other.disc;
    switch (other.disc) {
      case V:
        this.value = U3.FACTORY.create((U3) other.value);
        break;
    }
  }

  private U6(Disc disc, Object value) {
    this.disc = disc;
    this.value = value;
  }

  /* Accessors */

  public Disc getDisc() {
    return disc;
  }

  public U3 getV() {
    if (disc == Disc.V) {
      return (U3) value;
    }
    throw new IllegalStateException();
  }

  /* Mutators */

  public void setV(U3 v) {
    this.value = Objects.requireNonNull(v);
    this.disc = Disc.V;
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof U6)) {
      return false;
    }
    U6 other = (U6) other0;
    return disc == other.disc && value.equals(other.value);
  }

  @Override
  public int hashCode() {
    return disc.hashCode() * 37 + value.hashCode();
  }

  /* Factory for construction of generic values */

  public static final Factory<U6> FACTORY = new Factory<U6>() {
    public U6 create() {
      return new U6();
    }
    public U6 create(U6 other) {
      return new U6(other);
    }
  };

  /* Json serialization */

  public static JsonBinding<U6> jsonBinding() {
    final Lazy<JsonBinding<U3>> v = new Lazy<>(() -> U3.jsonBinding());
    final Factory<U6> _factory = FACTORY;

    return new JsonBinding<U6>() {
      public Factory<U6> factory() {
        return _factory;
      }

      public JsonElement toJson(U6 _value) {
        switch (_value.getDisc()) {
          case V:
            return JsonBindings.unionToJson("v", _value.getV(), v.get());
        }
        return null;
      }

      public U6 fromJson(JsonElement _json) {
        String _key = JsonBindings.unionNameFromJson(_json);
        if (_key.equals("v")) {
          return U6.v(JsonBindings.unionValueFromJson(_json, v.get()));
        }
        throw new IllegalStateException();
      }
    };
  }
}
