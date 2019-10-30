/* @generated from adl module test5 */

package adl.test5;

import com.google.gson.JsonElement;
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

public class U3 {

  /* Members */

  private Disc disc;
  private Object value;

  /**
   * The U3 discriminator type.
   */
  public enum Disc {
    V
  }

  /* Constructors */

  public static U3 v(short v) {
    return new U3(Disc.V, v);
  }

  public U3() {
    this.disc = Disc.V;
    this.value = (short)100;
  }

  public U3(U3 other) {
    this.disc = other.disc;
    switch (other.disc) {
      case V:
        this.value = (Short) other.value;
        break;
    }
  }

  private U3(Disc disc, Object value) {
    this.disc = disc;
    this.value = value;
  }

  /* Accessors */

  public Disc getDisc() {
    return disc;
  }

  public short getV() {
    if (disc == Disc.V) {
      return (Short) value;
    }
    throw new IllegalStateException();
  }

  /* Mutators */

  public void setV(short v) {
    this.value = v;
    this.disc = Disc.V;
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof U3)) {
      return false;
    }
    U3 other = (U3) other0;
    return disc == other.disc && value.equals(other.value);
  }

  @Override
  public int hashCode() {
    return disc.hashCode() * 37 + value.hashCode();
  }

  /* Factory for construction of generic values */

  public static final Factory<U3> FACTORY = new Factory<U3>() {
    @Override
    public U3 create() {
      return new U3();
    }

    @Override
    public U3 create(U3 other) {
      return new U3(other);
    }

    @Override
    public TypeExpr typeExpr() {
      ScopedName scopedName = new ScopedName("test5", "U3");
      ArrayList<TypeExpr> params = new ArrayList<>();
      return new TypeExpr(TypeRef.reference(scopedName), params);
    }

    @Override
    public JsonBinding<U3> jsonBinding() {
      return U3.jsonBinding();
    }
  };

  /* Json serialization */

  public static JsonBinding<U3> jsonBinding() {
    final Lazy<JsonBinding<Short>> v = new Lazy<>(() -> JsonBindings.INT16);
    final Factory<U3> _factory = FACTORY;

    return new JsonBinding<U3>() {
      @Override
      public Factory<U3> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(U3 _value) {
        switch (_value.getDisc()) {
          case V:
            return JsonBindings.unionToJson("v", _value.getV(), v.get());
        }
        return null;
      }

      @Override
      public U3 fromJson(JsonElement _json) {
        String _key = JsonBindings.unionNameFromJson(_json);
        if (_key.equals("v")) {
          return U3.v(JsonBindings.unionValueFromJson(_json, v.get()));
        }
        throw new JsonParseException("Invalid discriminator " + _key + " for union U3");
      }
    };
  }
}
