/* @generated from adl module test30 */

package adl.test30;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
import org.adl.runtime.Lazy;
import org.adl.runtime.sys.adlast.ScopedName;
import org.adl.runtime.sys.adlast.TypeExpr;
import org.adl.runtime.sys.adlast.TypeRef;
import java.util.ArrayList;
import java.util.Objects;

public class OldOutter {

  /* Members */

  private OrgField field0;

  /* Constructors */

  public OldOutter(OrgField field0) {
    this.field0 = Objects.requireNonNull(field0);
  }

  public OldOutter(OldOutter other) {
    this.field0 = OrgField.FACTORY.create(other.field0);
  }

  /* Accessors and mutators */

  public OrgField getField0() {
    return field0;
  }

  public OldOutter setField0(OrgField field0) {
    this.field0 = Objects.requireNonNull(field0);
    return this;
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof OldOutter)) {
      return false;
    }
    OldOutter other = (OldOutter) other0;
    return
      field0.equals(other.field0);
  }

  @Override
  public int hashCode() {
    int _result = 1;
    _result = _result * 37 + field0.hashCode();
    return _result;
  }

  /* Factory for construction of generic values */

  public static final Factory<OldOutter> FACTORY = new Factory<OldOutter>() {
    @Override
    public OldOutter create(OldOutter other) {
      return new OldOutter(other);
    }

    @Override
    public TypeExpr typeExpr() {
      ScopedName scopedName = new ScopedName("test30", "OldOutter");
      ArrayList<TypeExpr> params = new ArrayList<>();
      return new TypeExpr(TypeRef.reference(scopedName), params);
    }
    @Override
    public JsonBinding<OldOutter> jsonBinding() {
      return OldOutter.jsonBinding();
    }
  };

  /* Json serialization */

  public static JsonBinding<OldOutter> jsonBinding() {
    final Lazy<JsonBinding<OrgField>> field0 = new Lazy<>(() -> OrgField.jsonBinding());
    final Factory<OldOutter> _factory = FACTORY;

    return new JsonBinding<OldOutter>() {
      @Override
      public Factory<OldOutter> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(OldOutter _value) {
        JsonObject _result = new JsonObject();
        _result.add("field0", field0.get().toJson(_value.field0));
        return _result;
      }

      @Override
      public OldOutter fromJson(JsonElement _json) {
        JsonObject _obj = JsonBindings.objectFromJson(_json);
        return new OldOutter(
          JsonBindings.fieldFromJson(_obj, "field0", field0.get())
        );
      }
    };
  }
}
