/* @generated from adl module test30 */

package adl.test30;

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

public class Plain {

  /* Members */

  private Disc disc;
  private Object value;

  /**
   * The Plain discriminator type.
   */
  public enum Disc {
    A,
    B
  }

  /* Constructors */

  public static Plain a(String v) {
    return new Plain(Disc.A, Objects.requireNonNull(v));
  }

  public static Plain b(String v) {
    return new Plain(Disc.B, Objects.requireNonNull(v));
  }

  public Plain(Plain other) {
    this.disc = other.disc;
    switch (other.disc) {
      case A:
        this.value = (String) other.value;
        break;
      case B:
        this.value = (String) other.value;
        break;
    }
  }

  private Plain(Disc disc, Object value) {
    this.disc = disc;
    this.value = value;
  }

  /* Accessors */

  public Disc getDisc() {
    return disc;
  }

  public String getA() {
    if (disc == Disc.A) {
      return (String) value;
    }
    throw new IllegalStateException();
  }

  public String getB() {
    if (disc == Disc.B) {
      return (String) value;
    }
    throw new IllegalStateException();
  }

  /* Mutators */

  public void setA(String v) {
    this.value = Objects.requireNonNull(v);
    this.disc = Disc.A;
  }

  public void setB(String v) {
    this.value = Objects.requireNonNull(v);
    this.disc = Disc.B;
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof Plain)) {
      return false;
    }
    Plain other = (Plain) other0;
    return disc == other.disc && value.equals(other.value);
  }

  @Override
  public int hashCode() {
    return disc.hashCode() * 37 + value.hashCode();
  }

  /* Factory for construction of generic values */

  public static final Factory<Plain> FACTORY = new Factory<Plain>() {
    @Override
    public Plain create(Plain other) {
      return new Plain(other);
    }

    @Override
    public TypeExpr typeExpr() {
      ScopedName scopedName = new ScopedName("test30", "Plain");
      ArrayList<TypeExpr> params = new ArrayList<>();
      return new TypeExpr(TypeRef.reference(scopedName), params);
    }

    @Override
    public JsonBinding<Plain> jsonBinding() {
      return Plain.jsonBinding();
    }
  };

  /* Json serialization */

  public static JsonBinding<Plain> jsonBinding() {
    final Lazy<JsonBinding<String>> a = new Lazy<>(() -> JsonBindings.STRING);
    final Lazy<JsonBinding<String>> b = new Lazy<>(() -> JsonBindings.STRING);
    final Factory<Plain> _factory = FACTORY;

    return new JsonBinding<Plain>() {
      @Override
      public Factory<Plain> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(Plain _value) {
        switch (_value.getDisc()) {
          case A:
            return JsonBindings.unionToJson("a", _value.getA(), a.get());
          case B:
            return JsonBindings.unionToJson("b", _value.getB(), b.get());
        }
        return null;
      }

      @Override
      public Plain fromJson(JsonElement _json) {
        String _key = JsonBindings.unionNameFromJson(_json);
        if (_key.equals("a")) {
          return Plain.a(JsonBindings.unionValueFromJson(_json, a.get()));
        }
        else if (_key.equals("b")) {
          return Plain.b(JsonBindings.unionValueFromJson(_json, b.get()));
        }
        throw new JsonParseException("Invalid discriminator " + _key + " for union Plain");
      }
    };
  }
}
