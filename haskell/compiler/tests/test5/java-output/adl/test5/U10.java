/* @generated from adl module test5 */

package adl.test5;

import com.google.gson.JsonElement;
import org.adl.runtime.AdlVoid;
import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
import org.adl.runtime.JsonParseException;
import org.adl.runtime.Lazy;
import org.adl.runtime.sys.adlast.ScopedName;
import org.adl.runtime.sys.adlast.TypeExpr;
import org.adl.runtime.sys.adlast.TypeRef;
import java.util.ArrayList;
import java.util.Objects;

public class U10 {

  /* Members */

  private Disc disc;
  private Object value;

  /**
   * The U10 discriminator type.
   */
  public enum Disc {
    V1,
    V2
  }

  /* Constructors */

  public static U10 v1(short v) {
    return new U10(Disc.V1, v);
  }

  public static U10 v2() {
    return new U10(Disc.V2, null);
  }

  public U10(U10 other) {
    this.disc = other.disc;
    switch (other.disc) {
      case V1:
        this.value = (Short) other.value;
        break;
      case V2:
        this.value = (AdlVoid) other.value;
        break;
    }
  }

  private U10(Disc disc, Object value) {
    this.disc = disc;
    this.value = value;
  }

  /* Accessors */

  public Disc getDisc() {
    return disc;
  }

  public short getV1() {
    if (disc == Disc.V1) {
      return (Short) value;
    }
    throw new IllegalStateException();
  }

  /* Mutators */

  public void setV1(short v) {
    this.value = v;
    this.disc = Disc.V1;
  }

  public void setV2() {
    this.value = null;
    this.disc = Disc.V2;
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof U10)) {
      return false;
    }
    U10 other = (U10) other0;
    switch (disc) {
      case V1:
        return disc == other.disc && value.equals(other.value);
      case V2:
        return disc == other.disc;
    }
    throw new IllegalStateException();
  }

  @Override
  public int hashCode() {
    switch (disc) {
      case V1:
        return disc.hashCode() * 37 + value.hashCode();
      case V2:
        return disc.hashCode();
    }
    throw new IllegalStateException();
  }

  /* Factory for construction of generic values */

  public static final Factory<U10> FACTORY = new Factory<U10>() {
    @Override
    public U10 create(U10 other) {
      return new U10(other);
    }

    @Override
    public TypeExpr typeExpr() {
      ScopedName scopedName = new ScopedName("test5", "U10");
      ArrayList<TypeExpr> params = new ArrayList<>();
      return new TypeExpr(TypeRef.reference(scopedName), params);
    }

    @Override
    public JsonBinding<U10> jsonBinding() {
      return U10.jsonBinding();
    }
  };

  /* Json serialization */

  public static JsonBinding<U10> jsonBinding() {
    final Lazy<JsonBinding<Short>> v1 = new Lazy<>(() -> JsonBindings.INT16);
    final Lazy<JsonBinding<AdlVoid>> v2 = new Lazy<>(() -> JsonBindings.VOID);
    final Factory<U10> _factory = FACTORY;

    return new JsonBinding<U10>() {
      @Override
      public Factory<U10> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(U10 _value) {
        switch (_value.getDisc()) {
          case V1:
            return JsonBindings.unionToJson("v1", _value.getV1(), v1.get());
          case V2:
            return JsonBindings.unionToJson("v2", null, null);
        }
        return null;
      }

      @Override
      public U10 fromJson(JsonElement _json) {
        String _key = JsonBindings.unionNameFromJson(_json);
        if (_key.equals("v1")) {
          return U10.v1(JsonBindings.unionValueFromJson(_json, v1.get()));
        }
        else if (_key.equals("v2")) {
          return U10.v2();
        }
        throw new JsonParseException("Invalid discriminator " + _key + " for union U10");
      }
    };
  }
}
