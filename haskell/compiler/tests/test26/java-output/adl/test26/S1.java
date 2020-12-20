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

public class S1 {

  /* Members */

  private String f1;

  /* Constructors */

  public S1(String f1) {
    this.f1 = Objects.requireNonNull(f1);
  }

  public S1(S1 other) {
    this.f1 = other.f1;
  }

  /* Accessors and mutators */

  public String getF1() {
    return f1;
  }

  public S1 setF1(String f1) {
    this.f1 = Objects.requireNonNull(f1);
    return this;
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof S1)) {
      return false;
    }
    S1 other = (S1) other0;
    return
      f1.equals(other.f1);
  }

  @Override
  public int hashCode() {
    int _result = 1;
    _result = _result * 37 + f1.hashCode();
    return _result;
  }

  /* Factory for construction of generic values */

  public static final Factory<S1> FACTORY = new Factory<S1>() {
    @Override
    public S1 create(S1 other) {
      return new S1(other);
    }

    @Override
    public TypeExpr typeExpr() {
      ScopedName scopedName = new ScopedName("test26", "S1");
      ArrayList<TypeExpr> params = new ArrayList<>();
      return new TypeExpr(TypeRef.reference(scopedName), params);
    }
    @Override
    public JsonBinding<S1> jsonBinding() {
      return S1.jsonBinding();
    }
  };

  /* Json serialization */

  public static JsonBinding<S1> jsonBinding() {
    final Lazy<JsonBinding<String>> f1 = new Lazy<>(() -> JsonBindings.STRING);
    final Factory<S1> _factory = FACTORY;

    return new JsonBinding<S1>() {
      @Override
      public Factory<S1> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(S1 _value) {
        JsonObject _result = new JsonObject();
        _result.add("f1", f1.get().toJson(_value.f1));
        return _result;
      }

      @Override
      public S1 fromJson(JsonElement _json) {
        JsonObject _obj = JsonBindings.objectFromJson(_json);
        return new S1(
          JsonBindings.fieldFromJson(_obj, "f1", f1.get())
        );
      }
    };
  }
}
