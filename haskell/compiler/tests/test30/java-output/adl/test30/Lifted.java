/* @generated from adl module test30 */

package adl.test30;

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

public class Lifted {

  /* Members */

  private Disc disc;
  private Object value;

  /**
   * The Lifted discriminator type.
   */
  public enum Disc {
    ORG_FIELD,
    OPTION2
  }

  /* Constructors */

  public static Lifted org_field(OrgField v) {
    return new Lifted(Disc.ORG_FIELD, Objects.requireNonNull(v));
  }

  public static Lifted option2(AddedField v) {
    return new Lifted(Disc.OPTION2, Objects.requireNonNull(v));
  }

  public Lifted(Lifted other) {
    this.disc = other.disc;
    switch (other.disc) {
      case ORG_FIELD:
        this.value = OrgField.FACTORY.create((OrgField) other.value);
        break;
      case OPTION2:
        this.value = AddedField.FACTORY.create((AddedField) other.value);
        break;
    }
  }

  private Lifted(Disc disc, Object value) {
    this.disc = disc;
    this.value = value;
  }

  /* Accessors */

  public Disc getDisc() {
    return disc;
  }

  public OrgField getOrg_field() {
    if (disc == Disc.ORG_FIELD) {
      return (OrgField) value;
    }
    throw new IllegalStateException();
  }

  public AddedField getOption2() {
    if (disc == Disc.OPTION2) {
      return (AddedField) value;
    }
    throw new IllegalStateException();
  }

  /* Mutators */

  public void setOrg_field(OrgField v) {
    this.value = Objects.requireNonNull(v);
    this.disc = Disc.ORG_FIELD;
  }

  public void setOption2(AddedField v) {
    this.value = Objects.requireNonNull(v);
    this.disc = Disc.OPTION2;
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof Lifted)) {
      return false;
    }
    Lifted other = (Lifted) other0;
    return disc == other.disc && value.equals(other.value);
  }

  @Override
  public int hashCode() {
    return disc.hashCode() * 37 + value.hashCode();
  }

  /* Factory for construction of generic values */

  public static final Factory<Lifted> FACTORY = new Factory<Lifted>() {
    @Override
    public Lifted create(Lifted other) {
      return new Lifted(other);
    }

    @Override
    public TypeExpr typeExpr() {
      ScopedName scopedName = new ScopedName("test30", "Lifted");
      ArrayList<TypeExpr> params = new ArrayList<>();
      return new TypeExpr(TypeRef.reference(scopedName), params);
    }

    @Override
    public JsonBinding<Lifted> jsonBinding() {
      return Lifted.jsonBinding();
    }
  };

  /* Json serialization */

  public static JsonBinding<Lifted> jsonBinding() {
    final Lazy<JsonBinding<OrgField>> org_field = new Lazy<>(() -> OrgField.jsonBinding());
    final Lazy<JsonBinding<AddedField>> option2 = new Lazy<>(() -> AddedField.jsonBinding());
    final Factory<Lifted> _factory = FACTORY;

    return new JsonBinding<Lifted>() {
      @Override
      public Factory<Lifted> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(Lifted _value) {
        switch (_value.getDisc()) {
          case ORG_FIELD:
            return JsonBindings.unionToJson("org_field", _value.getOrg_field(), org_field.get());
          case OPTION2:
            return JsonBindings.unionToJson("option2", _value.getOption2(), option2.get());
        }
        return null;
      }

      private Lifted fromJsonUnion(JsonElement _json) {
        String _key = JsonBindings.unionNameFromJson(_json);
        if (_key.equals("org_field")) {
          return Lifted.org_field(JsonBindings.unionValueFromJson(_json, org_field.get()));
        }
        else if (_key.equals("option2")) {
          return Lifted.option2(JsonBindings.unionValueFromJson(_json, option2.get()));
        }
        throw new JsonParseException("Invalid discriminator " + _key + " for union Lifted");
      }

      @Override
      public Lifted fromJson(JsonElement _json) {
        try {
          return fromJsonUnion(_json);
        } catch (Exception e) {
          try {
            _json.getAsString();
          } catch (UnsupportedOperationException | ClassCastException e0) {
            try {
              return Lifted.org_field(org_field.get().fromJson(_json));
            } catch(JsonParseException e2) {
              throw e;
            }
          }
          throw new JsonParseException( "can't lift String or Void using AllowUntaggedDeserializeOfFirstBranch");
        }
      }
    };
  }
}
