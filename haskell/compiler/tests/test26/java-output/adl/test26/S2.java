/* @generated from adl module test26 */

package adl.test26;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
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

public class S2 {

  /* Members */

  private double f2;

  /* Constructors */

  public S2(double f2) {
    this.f2 = f2;
  }

  public S2() {
    this.f2 = 0.0;
  }

  public S2(S2 other) {
    this.f2 = other.f2;
  }

  /* Accessors and mutators */

  public double getF2() {
    return f2;
  }

  public void setF2(double f2) {
    this.f2 = f2;
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof S2)) {
      return false;
    }
    S2 other = (S2) other0;
    return
      f2 == other.f2;
  }

  @Override
  public int hashCode() {
    int _result = 1;
    _result = _result * 37 + Double.valueOf(f2).hashCode();
    return _result;
  }

  /* Factory for construction of generic values */

  public static final Factory<S2> FACTORY = new Factory<S2>() {
    @Override
    public S2 create() {
      return new S2();
    }

    @Override
    public S2 create(S2 other) {
      return new S2(other);
    }

    @Override
    public TypeExpr typeExpr() {
      ScopedName scopedName = new ScopedName("test26", "S2");
      ArrayList<TypeExpr> params = new ArrayList<>();
      return new TypeExpr(TypeRef.reference(scopedName), params);
    }
    @Override
    public JsonBinding<S2> jsonBinding() {
      return S2.jsonBinding();
    }
  };

  /* Json serialization */

  public static JsonBinding<S2> jsonBinding() {
    final Lazy<JsonBinding<Double>> f2 = new Lazy<>(() -> JsonBindings.DOUBLE);
    final Factory<S2> _factory = FACTORY;

    return new JsonBinding<S2>() {
      @Override
      public Factory<S2> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(S2 _value) {
        JsonObject _result = new JsonObject();
        _result.add("f2", f2.get().toJson(_value.f2));
        return _result;
      }

      @Override
      public S2 fromJson(JsonElement _json) {
        JsonObject _obj = JsonBindings.objectFromJson(_json);
        return new S2(
          JsonBindings.fieldFromJson(_obj, "f2", f2.get())
        );
      }
    };
  }
}
