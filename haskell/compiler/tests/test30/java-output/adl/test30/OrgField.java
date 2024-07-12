/* @generated from adl module test30 */

package adl.test30;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.adl.runtime.Builders;
import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
import org.adl.runtime.Lazy;
import org.adl.runtime.sys.adlast.ScopedName;
import org.adl.runtime.sys.adlast.TypeExpr;
import org.adl.runtime.sys.adlast.TypeRef;
import java.util.ArrayList;
import java.util.Objects;

public class OrgField {

  /* Members */

  private String a;
  private long b;

  /* Constructors */

  public OrgField(String a, long b) {
    this.a = Objects.requireNonNull(a);
    this.b = b;
  }

  public OrgField(OrgField other) {
    this.a = other.a;
    this.b = other.b;
  }

  /* Accessors and mutators */

  public String getA() {
    return a;
  }

  public OrgField setA(String a) {
    this.a = Objects.requireNonNull(a);
    return this;
  }

  public long getB() {
    return b;
  }

  public OrgField setB(long b) {
    this.b = b;
    return this;
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof OrgField)) {
      return false;
    }
    OrgField other = (OrgField) other0;
    return
      a.equals(other.a) &&
      b == other.b;
  }

  @Override
  public int hashCode() {
    int _result = 1;
    _result = _result * 37 + a.hashCode();
    _result = _result * 37 + (int) (b ^ (b >>> 32));
    return _result;
  }

  /* Builder */

  public static class Builder {
    private String a;
    private Long b;

    public Builder() {
      this.a = null;
      this.b = null;
    }

    public Builder setA(String a) {
      this.a = Objects.requireNonNull(a);
      return this;
    }

    public Builder setB(Long b) {
      this.b = Objects.requireNonNull(b);
      return this;
    }

    public OrgField create() {
      Builders.checkFieldInitialized("OrgField", "a", a);
      Builders.checkFieldInitialized("OrgField", "b", b);
      return new OrgField(a, b);
    }
  }

  /* Factory for construction of generic values */

  public static final Factory<OrgField> FACTORY = new Factory<OrgField>() {
    @Override
    public OrgField create(OrgField other) {
      return new OrgField(other);
    }

    @Override
    public TypeExpr typeExpr() {
      ScopedName scopedName = new ScopedName("test30", "OrgField");
      ArrayList<TypeExpr> params = new ArrayList<>();
      return new TypeExpr(TypeRef.reference(scopedName), params);
    }
    @Override
    public JsonBinding<OrgField> jsonBinding() {
      return OrgField.jsonBinding();
    }
  };

  /* Json serialization */

  public static JsonBinding<OrgField> jsonBinding() {
    final Lazy<JsonBinding<String>> a = new Lazy<>(() -> JsonBindings.STRING);
    final Lazy<JsonBinding<Long>> b = new Lazy<>(() -> JsonBindings.INT64);
    final Factory<OrgField> _factory = FACTORY;

    return new JsonBinding<OrgField>() {
      @Override
      public Factory<OrgField> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(OrgField _value) {
        JsonObject _result = new JsonObject();
        _result.add("a", a.get().toJson(_value.a));
        _result.add("b", b.get().toJson(_value.b));
        return _result;
      }

      @Override
      public OrgField fromJson(JsonElement _json) {
        JsonObject _obj = JsonBindings.objectFromJson(_json);
        return new OrgField(
          JsonBindings.fieldFromJson(_obj, "a", a.get()),
          JsonBindings.fieldFromJson(_obj, "b", b.get())
        );
      }
    };
  }
}
