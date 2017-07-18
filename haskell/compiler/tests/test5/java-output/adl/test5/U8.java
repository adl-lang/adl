/* Code generated from adl module test5 */

package adl.test5;

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

@SuppressWarnings("unused")
public class U8 {

  /* Members */

  private Disc disc;
  private Object value;

  /**
   * The U8 discriminator type.
   */
  public enum Disc {
    V1,
    V2
  }

  /* Constructors */

  public static U8 v1(S1 v) {
    return new U8(Disc.V1, Objects.requireNonNull(v));
  }

  public static U8 v2(short v) {
    return new U8(Disc.V2, v);
  }

  public U8() {
    this.disc = Disc.V1;
    this.value = new S1();
  }

  public U8(U8 other) {
    this.disc = other.disc;
    switch (other.disc) {
      case V1:
        this.value = S1.FACTORY.create((S1) other.value);
        break;
      case V2:
        this.value = (Short) other.value;
        break;
    }
  }

  private U8(Disc disc, Object value) {
    this.disc = disc;
    this.value = value;
  }

  /* Accessors */

  public Disc getDisc() {
    return disc;
  }

  public S1 getV1() {
    if (disc == Disc.V1) {
      return (S1) value;
    }
    throw new IllegalStateException();
  }

  public short getV2() {
    if (disc == Disc.V2) {
      return (Short) value;
    }
    throw new IllegalStateException();
  }

  /* Mutators */

  public void setV1(S1 v) {
    this.value = Objects.requireNonNull(v);
    this.disc = Disc.V1;
  }

  public void setV2(short v) {
    this.value = v;
    this.disc = Disc.V2;
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof U8)) {
      return false;
    }
    U8 other = (U8) other0;
    return disc == other.disc && value.equals(other.value);
  }

  @Override
  public int hashCode() {
    return disc.hashCode() * 37 + value.hashCode();
  }

  /* Factory for construction of generic values */

  public static final Factory<U8> FACTORY = new Factory<U8>() {
    @Override
    public U8 create() {
      return new U8();
    }

    @Override
    public U8 create(U8 other) {
      return new U8(other);
    }
  };

  /* Json serialization */

  public static JsonBinding<U8> jsonBinding() {
    final Lazy<JsonBinding<S1>> v1 = new Lazy<>(() -> S1.jsonBinding());
    final Lazy<JsonBinding<Short>> v2 = new Lazy<>(() -> JsonBindings.SHORT);
    final Factory<U8> _factory = FACTORY;

    return new JsonBinding<U8>() {
      public Factory<U8> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(U8 _value) {
        switch (_value.getDisc()) {
          case V1:
            return JsonBindings.unionToJson("v1", _value.getV1(), v1.get());
          case V2:
            return JsonBindings.unionToJson("v2", _value.getV2(), v2.get());
        }
        return null;
      }

      @Override
      public U8 fromJson(JsonElement _json) {
        String _key = JsonBindings.unionNameFromJson(_json);
        if (_key.equals("v1")) {
          return U8.v1(JsonBindings.unionValueFromJson(_json, v1.get()));
        }
        else if (_key.equals("v2")) {
          return U8.v2(JsonBindings.unionValueFromJson(_json, v2.get()));
        }
        throw new JsonParseException("Invalid discriminator " + _key + " for union U8");
      }
    };
  }
}
