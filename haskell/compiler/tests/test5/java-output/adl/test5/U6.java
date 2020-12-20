/* @generated from adl module test5 */

package adl.test5;

import com.google.gson.JsonElement;
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
    @Override
    public U6 create(U6 other) {
      return new U6(other);
    }

    @Override
    public TypeExpr typeExpr() {
      ScopedName scopedName = new ScopedName("test5", "U6");
      ArrayList<TypeExpr> params = new ArrayList<>();
      return new TypeExpr(TypeRef.reference(scopedName), params);
    }

    @Override
    public JsonBinding<U6> jsonBinding() {
      return U6.jsonBinding();
    }
  };

  /* Json serialization */

  public static JsonBinding<U6> jsonBinding() {
    final Lazy<JsonBinding<U3>> v = new Lazy<>(() -> U3.jsonBinding());
    final Factory<U6> _factory = FACTORY;

    return new JsonBinding<U6>() {
      @Override
      public Factory<U6> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(U6 _value) {
        switch (_value.getDisc()) {
          case V:
            return JsonBindings.unionToJson("v", _value.getV(), v.get());
        }
        return null;
      }

      @Override
      public U6 fromJson(JsonElement _json) {
        String _key = JsonBindings.unionNameFromJson(_json);
        if (_key.equals("v")) {
          return U6.v(JsonBindings.unionValueFromJson(_json, v.get()));
        }
        throw new JsonParseException("Invalid discriminator " + _key + " for union U6");
      }
    };
  }
}
