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

public class U5 {

  /* Members */

  private Disc disc;
  private Object value;

  /**
   * The U5 discriminator type.
   */
  public enum Disc {
    V
  }

  /* Constructors */

  public static U5 v(S1 v) {
    return new U5(Disc.V, Objects.requireNonNull(v));
  }

  public U5(U5 other) {
    this.disc = other.disc;
    switch (other.disc) {
      case V:
        this.value = S1.FACTORY.create((S1) other.value);
        break;
    }
  }

  private U5(Disc disc, Object value) {
    this.disc = disc;
    this.value = value;
  }

  /* Accessors */

  public Disc getDisc() {
    return disc;
  }

  public S1 getV() {
    if (disc == Disc.V) {
      return (S1) value;
    }
    throw new IllegalStateException();
  }

  /* Mutators */

  public void setV(S1 v) {
    this.value = Objects.requireNonNull(v);
    this.disc = Disc.V;
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof U5)) {
      return false;
    }
    U5 other = (U5) other0;
    return disc == other.disc && value.equals(other.value);
  }

  @Override
  public int hashCode() {
    return disc.hashCode() * 37 + value.hashCode();
  }

  /* Factory for construction of generic values */

  public static final Factory<U5> FACTORY = new Factory<U5>() {
    @Override
    public U5 create(U5 other) {
      return new U5(other);
    }

    @Override
    public TypeExpr typeExpr() {
      ScopedName scopedName = new ScopedName("test5", "U5");
      ArrayList<TypeExpr> params = new ArrayList<>();
      return new TypeExpr(TypeRef.reference(scopedName), params);
    }

    @Override
    public JsonBinding<U5> jsonBinding() {
      return U5.jsonBinding();
    }
  };

  /* Json serialization */

  public static JsonBinding<U5> jsonBinding() {
    final Lazy<JsonBinding<S1>> v = new Lazy<>(() -> S1.jsonBinding());
    final Factory<U5> _factory = FACTORY;

    return new JsonBinding<U5>() {
      @Override
      public Factory<U5> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(U5 _value) {
        switch (_value.getDisc()) {
          case V:
            return JsonBindings.unionToJson("v", _value.getV(), v.get());
        }
        return null;
      }

      @Override
      public U5 fromJson(JsonElement _json) {
        String _key = JsonBindings.unionNameFromJson(_json);
        if (_key.equals("v")) {
          return U5.v(JsonBindings.unionValueFromJson(_json, v.get()));
        }
        throw new JsonParseException("Invalid discriminator " + _key + " for union U5");
      }
    };
  }
}
