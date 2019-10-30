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

public class U2 {

  /* Members */

  private Disc disc;
  private Object value;

  /**
   * The U2 discriminator type.
   */
  public enum Disc {
    V
  }

  /* Constructors */

  public static U2 v(short v) {
    return new U2(Disc.V, v);
  }

  public U2() {
    this.disc = Disc.V;
    this.value = (short)0;
  }

  public U2(U2 other) {
    this.disc = other.disc;
    switch (other.disc) {
      case V:
        this.value = (Short) other.value;
        break;
    }
  }

  private U2(Disc disc, Object value) {
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
    if (!(other0 instanceof U2)) {
      return false;
    }
    U2 other = (U2) other0;
    return disc == other.disc && value.equals(other.value);
  }

  @Override
  public int hashCode() {
    return disc.hashCode() * 37 + value.hashCode();
  }

  /* Factory for construction of generic values */

  public static final Factory<U2> FACTORY = new Factory<U2>() {
    @Override
    public U2 create() {
      return new U2();
    }

    @Override
    public U2 create(U2 other) {
      return new U2(other);
    }

    @Override
    public TypeExpr typeExpr() {
      ScopedName scopedName = new ScopedName("test5", "U2");
      ArrayList<TypeExpr> params = new ArrayList<>();
      return new TypeExpr(TypeRef.reference(scopedName), params);
    }

    @Override
    public JsonBinding<U2> jsonBinding() {
      return U2.jsonBinding();
    }
  };

  /* Json serialization */

  public static JsonBinding<U2> jsonBinding() {
    final Lazy<JsonBinding<Short>> v = new Lazy<>(() -> JsonBindings.INT16);
    final Factory<U2> _factory = FACTORY;

    return new JsonBinding<U2>() {
      @Override
      public Factory<U2> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(U2 _value) {
        switch (_value.getDisc()) {
          case V:
            return JsonBindings.unionToJson("v", _value.getV(), v.get());
        }
        return null;
      }

      @Override
      public U2 fromJson(JsonElement _json) {
        String _key = JsonBindings.unionNameFromJson(_json);
        if (_key.equals("v")) {
          return U2.v(JsonBindings.unionValueFromJson(_json, v.get()));
        }
        throw new JsonParseException("Invalid discriminator " + _key + " for union U2");
      }
    };
  }
}
