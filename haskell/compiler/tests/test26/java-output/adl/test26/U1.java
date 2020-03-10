/* @generated from adl module test26 */

package adl.test26;

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

public class U1 {

  /* Members */

  private Disc disc;
  private Object value;

  /**
   * The U1 discriminator type.
   */
  public enum Disc {
    S1,
    S2
  }

  /* Constructors */

  public static U1 s1(S1 v) {
    return new U1(Disc.S1, Objects.requireNonNull(v));
  }

  public static U1 s2(S2 v) {
    return new U1(Disc.S2, Objects.requireNonNull(v));
  }

  public U1() {
    this.disc = Disc.S1;
    this.value = new S1();
  }

  public U1(U1 other) {
    this.disc = other.disc;
    switch (other.disc) {
      case S1:
        this.value = S1.FACTORY.create((S1) other.value);
        break;
      case S2:
        this.value = S2.FACTORY.create((S2) other.value);
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

  public S1 getS1() {
    if (disc == Disc.S1) {
      return (S1) value;
    }
    throw new IllegalStateException();
  }

  public S2 getS2() {
    if (disc == Disc.S2) {
      return (S2) value;
    }
    throw new IllegalStateException();
  }

  /* Mutators */

  public void setS1(S1 v) {
    this.value = Objects.requireNonNull(v);
    this.disc = Disc.S1;
  }

  public void setS2(S2 v) {
    this.value = Objects.requireNonNull(v);
    this.disc = Disc.S2;
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof U1)) {
      return false;
    }
    U1 other = (U1) other0;
    return disc == other.disc && value.equals(other.value);
  }

  @Override
  public int hashCode() {
    return disc.hashCode() * 37 + value.hashCode();
  }

  /* Factory for construction of generic values */

  public static final Factory<U1> FACTORY = new Factory<U1>() {
    @Override
    public U1 create() {
      return new U1();
    }

    @Override
    public U1 create(U1 other) {
      return new U1(other);
    }

    @Override
    public TypeExpr typeExpr() {
      ScopedName scopedName = new ScopedName("test26", "U1");
      ArrayList<TypeExpr> params = new ArrayList<>();
      return new TypeExpr(TypeRef.reference(scopedName), params);
    }

    @Override
    public JsonBinding<U1> jsonBinding() {
      return U1.jsonBinding();
    }
  };

  /* Json serialization */

  public static JsonBinding<U1> jsonBinding() {
    final Lazy<JsonBinding<S1>> s1 = new Lazy<>(() -> S1.jsonBinding());
    final Lazy<JsonBinding<S2>> s2 = new Lazy<>(() -> S2.jsonBinding());
    final Factory<U1> _factory = FACTORY;

    return new JsonBinding<U1>() {
      @Override
      public Factory<U1> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(U1 _value) {
        switch (_value.getDisc()) {
          case S1:
            return JsonBindings.unionToJson("s1", _value.getS1(), s1.get());
          case S2:
            return JsonBindings.unionToJson("s2", _value.getS2(), s2.get());
        }
        return null;
      }

      @Override
      public U1 fromJson(JsonElement _json) {
        String _key = JsonBindings.unionNameFromJson(_json);
        if (_key.equals("s1")) {
          return U1.s1(JsonBindings.unionValueFromJson(_json, s1.get()));
        }
        else if (_key.equals("s2")) {
          return U1.s2(JsonBindings.unionValueFromJson(_json, s2.get()));
        }
        throw new JsonParseException("Invalid discriminator " + _key + " for union U1");
      }
    };
  }
}
